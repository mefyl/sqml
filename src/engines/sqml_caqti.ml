open Acid
open Sqml
open Utils
module Array = Array.Monadic2 (Lwt_result)

open Let.Syntax2 (Lwt_result)

module Make (Model : Model.S) = struct
  type t = { pool : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t }

  type transaction = (module Caqti_lwt.CONNECTION)

  let quote_pattern = String.Search_pattern.create "'"

  let pp_literal_string f value =
    let open Fmt in
    let fmt = const char '\'' ++ string ++ const char '\'' in
    fmt f
      (String.Search_pattern.replace_all quote_pattern ~in_:value ~with_:"''")

  let rec pp_from_type : type t. t Sqml.Expression.Type.t -> t Fmt.t =
    let open Fmt in
    let rec pp_values :
        type l. l Hlist.t Sqml.Expression.Type.t -> l Hlist.t Fmt.t = function
      | [] -> nop
      | [ hd ] -> fun f [ hd_v ] -> pp_from_type hd f hd_v
      | hd :: tl ->
        fun f (hd_v :: tl_v) ->
          pp_from_type hd f hd_v;
          comma f ();
          pp_values tl f tl_v
      | _ -> failwith "FIXME"
    in
    function
    | Bool -> Fmt.bool
    | Date -> const char '\'' ++ Timmy.Date.pp ++ const char '\''
    | Int -> Fmt.int
    | Nullable t -> Fmt.(option ~none:(const string "NULL") @@ pp_from_type t)
    | String -> pp_literal_string
    | Time ->
      fun fmt t ->
        char fmt '\'';
        string fmt (Timmy.Time.to_rfc3339 t);
        char fmt '\''
    | Custom { encode; repr; _ } -> (
      fun f value ->
        match encode value with
        | Result.Ok value -> pp_from_type repr f value
        | Result.Error e -> (* FIXME *) failwith "pp_from_type: " e)
    | [] -> const string "VALUES ()"
    | _ :: _ as l -> const string "VALUES (" ++ pp_values l ++ const char ')'

  and pp_expression =
    let open Fmt in
    let rec pp_var : type t. t Expression.Type.t -> unit Fmt.t =
      let rec pp_values : type l. l Hlist.t Sqml.Expression.Type.t Fmt.t =
       fun f -> function
        | [] -> ()
        | [ _ ] -> const char '?' f ()
        | _ :: tl -> (const char '?' ++ comma ++ const pp_values tl) f ()
        | _ -> failwith "FIXME"
      in
      function
      | Custom { repr; _ } -> pp_var repr
      | [] -> const string "VALUES ()"
      | [ _ ] -> const string "VALUES (?)"
      | _ :: _ as t ->
        const string "VALUES (" ++ const pp_values t ++ const char ')'
      | Bool
      | Date
      | Int
      | Nullable _
      | String
      | Time ->
        const string "?"
    in
    let rec pp : type a. (a, 'tag) Expression.t Fmt.t =
     fun f -> function
      | Column { selectable; name; _ } ->
        (const string selectable ++ const string "." ++ const string name) f ()
      | Concat (l, r) ->
        let fmt = const pp l ++ sp ++ const string "||" ++ sp ++ const pp r in
        fmt f ()
      | Comparison (op, l, r) ->
        let fmt = const pp l ++ sp ++ pp_operator op ++ sp ++ const pp r in
        fmt f ()
      | Const { type_; value } -> pp_from_type type_ f value
      | Logic (op, l, r) ->
        let fmt = const pp l ++ sp ++ pp_logic op ++ sp ++ const pp r in
        fmt f ()
      | Map { mapped; _ } -> pp f mapped
      | Null -> Fmt.string f "NULL"
      | Var t -> pp_var t f ()
      | [] -> ()
      | [ a ] -> pp f a
      | a :: b -> (const pp a ++ const string ", " ++ const pp b) f ()
    in
    pp

  and pp_operator =
    let open Fmt in
    function
    | Eq -> const char '='
    | Gt -> const char '>'
    | Gte -> const string ">="
    | Lt -> const char '<'
    | Lte -> const string "<="

  and pp_logic =
    let open Fmt in
    function
    | And -> const string "AND"
    | Or -> const string "OR"

  let table_name info schema name =
    match schema with
    | None -> Format.sprintf "%S" name
    | Some schema -> (
      match Caqti_driver_info.dialect_tag info with
      | `Sqlite ->
        Format.sprintf "%s_%s" (String.escaped schema) (String.escaped name)
      | `Pgsql -> Format.sprintf "%S.%S" schema name
      | _ -> failwith "dialect not handled")

  let nullable : type t. t Expression.Column.Type.t -> bool = function
    | Nullable _ -> true
    | _ -> false

  let string_of_type info t =
    let type_ =
      match Caqti_driver_info.dialect_tag info with
      | `Sqlite -> Dialect.Sqlite.string_of_type t
      | _ -> Dialect.Postgresql.string_of_type t
    in
    if nullable t then
      Fmt.str "%s NULL" type_
    else
      Fmt.str "%s NOT NULL" type_

  let fail fmt =
    Caml.Format.ksprintf (fun s -> Lwt_result.fail (`Sqml_engine s)) fmt

  let convert_error =
    let f e = `Sqml_engine (Caqti_error.show e) in
    Result.map_error ~f

  let error = Lwt.map ~f:convert_error

  type 'res column_mapper = { f : 't. 't Sqml.Column.t -> 'res option }

  let column_map (f : 'res column_mapper) =
    let rec map : type t. t Column.List.t -> 'res list = function
      | Column.List.(column :: tl) -> (
        match f.f column with
        | Some value -> value :: map tl
        | None -> map tl)
      | [] -> []
    in
    map

  let columns i columns =
    let f { Column.name; type_; _ } =
      Option.return @@ Fmt.str "%s %s" name (string_of_type i type_)
    in
    column_map { f } columns

  and primary columns =
    let f { Column.name; primary; _ } =
      if primary then
        Some name
      else
        None
    in
    column_map { f } columns

  let make ?max_size uri =
    let* pool =
      Caqti_lwt.connect_pool ?max_size (Uri.of_string uri)
      |> Lwt.return |> error
    in
    let create (module C : Caqti_lwt.CONNECTION) =
      let i = C.driver_info in
      if not @@ Caqti_driver_info.can_transact i then
        Lwt_result.return (false, (-1, -1, -1))
      else
        let found_query =
          match Caqti_driver_info.dialect_tag i with
          | `Sqlite ->
            Caqti_request.find Caqti_type.unit Caqti_type.bool
              "SELECT COUNT(*) > 0 FROM sqlite_master WHERE type='table' AND \
               name= 'sqml_metadata'"
          | `Pgsql ->
            Caqti_request.find Caqti_type.unit Caqti_type.bool
              "SELECT EXISTS (SELECT FROM information_schema.tables WHERE \
               table_schema = 'sqml' AND table_name = 'metadata')"
          | _ -> failwith "dialect not handled"
        in
        let* found = C.find found_query () in
        let caqti_metadata_type = Caqti_type.(tup4 int int int ptime) in
        if found then
          let query =
            Caqti_request.find Caqti_type.unit caqti_metadata_type
              (Format.sprintf
                 "SELECT major, minor, subminor, migrated FROM %s ORDER BY %s \
                  DESC LIMIT 1"
                 (table_name i (Some "sqml") "metadata")
                 (if Poly.(Caqti_driver_info.dialect_tag i = `Sqlite) then
                   "datetime(migrated)"
                 else
                   "migrated"))
          in
          let* major, minor, subminor, _ = C.find query () in
          let current = (major, minor, subminor) in
          let* () =
            Log.info (fun m ->
                m "load model at version %s" @@ Semver.to_string current)
          in
          Lwt_result.return (true, current)
        else
          let* () =
            Log.info (fun m ->
                m "create model at version %s" @@ Semver.to_string Model.version)
          in
          let create_table (module T : Table.S) =
            let* () =
              match Caqti_driver_info.dialect_tag i with
              | `Sqlite -> Lwt_result.return ()
              | `Pgsql -> (
                match T.Meta.schema with
                | None -> Lwt_result.return ()
                | Some schema ->
                  let query =
                    Caqti_request.exec ~oneshot:true Caqti_type.unit
                      (Format.sprintf "CREATE SCHEMA IF NOT EXISTS %S" schema)
                  in
                  C.exec query ())
              | _ -> failwith "dialect not handled"
            in
            let columns = columns i T.Meta.columns in
            let columns =
              match primary T.Meta.columns with
              | [] -> columns
              | primary ->
                columns
                @ [
                    Fmt.str "PRIMARY KEY (%s)"
                    @@ String.concat ~sep:", " primary;
                  ]
            in
            let columns =
              let f = function
                | Table.Foreign_key references ->
                  let f (Table.Foreign_reference { target; column }) =
                    (column.name, target.column.name)
                  in
                  let columns, references =
                    Table.Nonempty_list.to_list references
                    |> List.map ~f |> List.unzip
                  and target_table =
                    let (Table.Foreign_reference { target; _ }) =
                      Table.Nonempty_list.hd_nonempty references
                    in
                    Table.name target.table
                  in
                  Fmt.(
                    str "FOREIGN KEY (%a) REFERENCES %s(%a)"
                      (list ~sep:comma string) columns target_table
                      (list ~sep:comma string) references)
              in
              columns @ List.map ~f T.constraints
            in
            let query =
              Fmt.str "CREATE TABLE %s (%s)"
                (table_name i T.Meta.schema T.Meta.name)
                (String.concat ~sep:", " columns)
            in
            let* () = Log.debug (fun m -> m "create table: %s" query) in
            let query =
              Caqti_request.exec ~oneshot:true Caqti_type.unit query
            in
            C.exec query ()
          in
          let* () = Array.iter ~f:create_table (Model.tables ()) in
          let query =
            Caqti_request.exec caqti_metadata_type
              (Format.sprintf
                 "INSERT INTO %s (major, minor, subminor, migrated) VALUES (?, \
                  ?, ?, ?)"
                 (table_name i (Some "sqml") "metadata"))
          in
          let* () =
            let major, minor, subminor = Model.version
            and now = Option.value_exn (Ptime.of_float_s @@ Unix.time ()) in
            C.exec query (major, minor, subminor, now)
          in
          Lwt_result.return (true, Model.version)
    in
    let* transaction, current = Caqti_lwt.Pool.use create pool |> error in
    if transaction then
      if Poly.(current <> Model.version) then
        fail "model version mismatch: code at %s, data at %s"
          (Semver.to_string Model.version)
          (Semver.to_string current)
      else
        Lwt_result.return { pool }
    else
      fail "backend does not support transactions"

  let close { pool } = Caqti_lwt.Pool.drain pool

  module Model = Model

  let rec type_of_column_type :
      type t. t Expression.Column.Type.t -> t Caqti_type.t = function
    | Bool -> Caqti_type.bool
    | Custom { encode; decode; repr } ->
      Caqti_type.custom ~decode ~encode @@ type_of_column_type repr
    | Date ->
      let encode d =
        Timmy.Date.to_time ~timezone:Timmy.Timezone.utc d
        |> Timmy.Time.to_ptime |> Result.return
      and decode t =
        Timmy.Time.of_ptime t
        |> Timmy.Date.of_time ~timezone:Timmy.Timezone.utc
        |> Result.return
      in
      Caqti_type.custom ~decode ~encode @@ Caqti_type.pdate
    | Int -> Caqti_type.int
    | Nullable t -> Caqti_type.option (type_of_column_type t)
    | Text -> Caqti_type.string
    | Time ->
      let encode t = Result.return @@ Timmy.Time.to_ptime t
      and decode t = Result.return @@ Timmy.Time.of_ptime t in
      Caqti_type.custom ~decode ~encode @@ Caqti_type.ptime
    | Varchar _ -> Caqti_type.string

  let rec type_of_type : type t. t Sqml.Expression.Type.t -> t Caqti_type.t =
    function
    | Bool -> Caqti_type.bool
    | Date ->
      let encode d =
        Timmy.Date.to_time ~timezone:Timmy.Timezone.utc d
        |> Timmy.Time.to_ptime |> Result.return
      and decode t =
        Timmy.Time.of_ptime t
        |> Timmy.Date.of_time ~timezone:Timmy.Timezone.utc
        |> Result.return
      in
      Caqti_type.custom ~decode ~encode @@ Caqti_type.pdate
    | Int -> Caqti_type.int
    | Nullable t -> Caqti_type.option (type_of_type t)
    | String -> Caqti_type.string
    | Time ->
      let encode t = Result.return @@ Timmy.Time.to_ptime t
      and decode t = Result.return @@ Timmy.Time.of_ptime t in
      Caqti_type.custom ~decode ~encode @@ Caqti_type.ptime
    | Custom { encode; decode; repr } ->
      Caqti_type.custom ~decode ~encode @@ type_of_type repr
    | [] ->
      let encode Hlist.[] = Result.return ()
      and decode () = Result.return Hlist.[] in
      Caqti_type.custom ~decode ~encode @@ Caqti_type.unit
    | l :: r ->
      let encode Hlist.(hd :: tl) = Result.return @@ (hd, tl)
      and decode (hd, tl) = Result.return @@ Hlist.(hd :: tl) in
      Caqti_type.custom ~decode ~encode
      @@ Caqti_type.tup2 (type_of_type l) (type_of_type r)

  let rec type_of_expr : type a. (a, _) Expression.t -> a Caqti_type.t =
    function
    | Column { type_; _ } -> type_of_column_type type_
    | Comparison _ -> Caqti_type.bool
    | Concat _ -> Caqti_type.string
    | Const { type_; _ } -> type_of_type type_
    | Logic _ -> Caqti_type.bool
    | Map { decode; encode; mapped } ->
      Caqti_type.custom
        ~decode:(fun x -> Result.Ok (decode x))
        ~encode:(fun x -> Result.Ok (encode x))
      @@ type_of_expr mapped
    | a :: b ->
      Caqti_type.custom
        ~decode:(fun (hd, tl) -> Result.Ok Hlist.(hd :: tl))
        ~encode:(fun Hlist.(hd :: tl) -> Result.Ok (hd, tl))
      @@ Caqti_type.tup2 (type_of_expr a) (type_of_expr b)
    | [] ->
      Caqti_type.custom
        ~decode:(fun () -> Result.Ok Hlist.[])
        ~encode:(fun Hlist.[] -> Result.Ok ())
      @@ Caqti_type.unit
    | Null -> failwith "can't deduce type of NULL"
    | Var t -> type_of_type t

  let rec type_of_expressions :
      type t tag. (t, tag) Expression.List.t -> t Hlist.t Caqti_type.t =
    function
    | Expression.List.[] ->
      Caqti_type.custom
        ~decode:(fun () -> Result.Ok Hlist.[])
        ~encode:(fun Hlist.[] -> Result.Ok ())
      @@ Caqti_type.unit
    | c :: tl ->
      Caqti_type.custom
        ~decode:(fun (hd, tl) -> Result.Ok Hlist.(hd :: tl))
        ~encode:(fun Hlist.(hd :: tl) -> Result.Ok (hd, tl))
      @@ Caqti_type.tup2 (type_of_expr c) (type_of_expressions tl)

  let query_type : type t arity. (t, arity) Model.Query.t -> t Caqti_type.t =
    function
    | Delete _ -> Caqti_type.unit
    | Delete_returning { returning; _ } -> type_of_expr returning
    | Insert _ -> Caqti_type.unit
    | Insert_returning { returning; _ } -> type_of_expr returning
    | Select { what; _ } -> type_of_expr what
    | Update _ -> Caqti_type.unit
    | Update_returning { returning; _ } -> type_of_expr returning

  let pp_assignment : type tag. tag Sqml.Expression.assignment Fmt.t =
   fun fmt (Sqml.Expression.Set { column; value }) ->
    Fmt.pf fmt "%s = %a" column.name pp_expression value

  let pp_query driver_info =
    let pp : type tag arity. (tag, arity) Model.Query.t Fmt.t =
     fun f ->
      let open Fmt in
      let pp_returning : type t tag. (t, tag) Expression.option Fmt.t =
       fun f -> function
        | Expression.None -> ()
        | Some e ->
          Fmt.(sp ++ const string "RETURNING" ++ sp ++ pp_expression) f e
      in
      let pp_insert
          (type mapped res tag)
          (module T : Table.S
            with type model = Model.tag
             and type tag = tag
             and type Meta.mapped = mapped)
          ({ excluded } : tag Model.Query.excluded)
          (on_conflict : tag Model.Query.on_conflict option)
          (returning : (res, tag) Expression.option)
          value =
        let dialect =
          match Caqti_driver_info.dialect_tag driver_info with
          | #Caqti_driver_info.sql_dialect_tag as t -> t
          | _ -> failwith "unsupported dialect"
        in
        let conflict_sqlite, conflict_pg =
          let open Fmt in
          match (dialect, on_conflict) with
          | _, None -> (nop, nop)
          | `Pgsql, Some Ignore -> (nop, const string " ON CONFLICT DO NOTHING")
          | `Pgsql, Some (Set assignments) ->
            ( nop,
              const string " ON CONFLICT ("
              ++ const (list ~sep:comma string) (primary T.Meta.columns)
              ++ const string ") DO UPDATE SET "
              ++ const
                   (list ~sep:comma pp_assignment)
                   (assignments { excluded }) )
          | `Pgsql, Some Update ->
            let f { Column.name; _ } =
              Option.return @@ Fmt.str "%s = excluded.%s" name name
            in
            ( nop,
              const string " ON CONFLICT ("
              ++ const (list ~sep:comma string) (primary T.Meta.columns)
              ++ const string ") DO UPDATE SET "
              ++ const (list ~sep:comma string)
                   (column_map { f } T.Meta.columns) )
          | `Sqlite, Some Ignore -> (const string " OR IGNORE", nop)
          | `Sqlite, Some Update -> (const string " OR REPLACE", nop)
          | `Sqlite, Some (Set _)
          | `Mysql, _ ->
            failwith "dialect not handled"
        in
        Fmt.pf f "INSERT%a INTO %S %a%a%a" conflict_sqlite () T.Meta.name
          pp_expression value conflict_pg () pp_returning returning
      and pp_update
          (type res tag)
          (module T : Table.S with type model = Model.tag and type tag = tag)
          (what : tag Expression.assignment list)
          (where : (bool, tag) Expression.t option)
          (returning : (res, tag) Expression.option) =
        (box
        @@ const string "UPDATE" ++ sp ++ const T.pp () ++ sp
           ++ const string "SET" ++ sp
           ++ const (list ~sep:comma pp_assignment) what
           ++ const
                (option (sp ++ const string "WHERE" ++ sp ++ pp_expression))
                where
           ++ const pp_returning returning)
          f ()
      in

      let pp_clause keyword f e = Fmt.pf f "@ %s %a" keyword pp_expression e in
      function
      | Delete { from; where } ->
        let module Table = (val from) in
        Fmt.pf f "DELETE FROM@ %a%a" Table.pp ()
          (option (pp_clause "WHERE"))
          where
      | Delete_returning { from; returning; where } ->
        let module Table = (val from) in
        Fmt.pf f "DELETE FROM@ %a%a%a" Table.pp ()
          (option (pp_clause "WHERE"))
          where (pp_clause "RETURNING") returning
      | Insert { excluded; into; on_conflict; value } ->
        pp_insert into { excluded } on_conflict Expression.None value
      | Insert_returning { excluded; into; on_conflict; returning; value } ->
        pp_insert into { excluded } on_conflict
          Expression.(Some returning)
          value
      | Select { from; what; where } ->
        let module Selectable = (val from) in
        (box
        @@ const string "SELECT" ++ sp ++ const pp_expression what ++ sp
           ++ const string "FROM" ++ sp ++ const Selectable.pp ()
           ++ const
                (option (sp ++ const string "WHERE" ++ sp ++ pp_expression))
                where)
          f ()
      | Update { into; what; where } ->
        pp_update into what where Expression.None
      | Update_returning { into; what; where; returning } ->
        pp_update into what where Expression.(Some returning)
    in

    pp

  let query
      (module C : Caqti_lwt.CONNECTION)
      (type res arity)
      (query : (res, arity) Model.Query.t) :
      ((res, arity, _) Model.Query.result, _) Lwt_result.t =
    let collect query =
      let f response =
        let f stream =
          Lwt.bind (stream ()) ~f:(function
            | Caqti_lwt.Stream.Nil -> Lwt_result.return None
            | Error e -> Lwt_result.fail e |> error
            | Cons (v, next) -> Lwt_result.return @@ Some (v, next))
        and init = C.Response.to_stream response in
        Lwt_result.return @@ Model.Query.Many (Sequence.unfold_step ~init ~f)
      in
      C.call ~f query ()
    in
    let query_str = Fmt.str "%a" (pp_query C.driver_info) query in
    let* () = Log.debug @@ fun m -> m "caqti: execute query: %s" query_str in
    let res : ((res, arity, _) Model.Query.result, _) Lwt_result.t =
      match query with
      | Delete _ ->
        let query = Caqti_request.exec Caqti_type.unit query_str in
        let+ () = C.exec query () in
        Model.Query.Zero
      | Delete_returning { returning; _ } ->
        let query =
          Caqti_request.collect Caqti_type.unit (type_of_expr returning)
            query_str
        in
        collect query
      | Select { what; _ } ->
        let query =
          Caqti_request.collect Caqti_type.unit (type_of_expr what) query_str
        in
        collect query
      | Insert _ ->
        let query = Caqti_request.exec Caqti_type.unit query_str in
        let+ () = C.exec query () in
        Model.Query.Zero
      | Insert_returning { returning; _ } ->
        let query =
          Caqti_request.find Caqti_type.unit (type_of_expr returning) query_str
        in
        let+ res = C.find query () in
        Model.Query.One res
      | Update _ ->
        let query = Caqti_request.exec Caqti_type.unit query_str in
        let+ () = C.exec query () in
        Model.Query.Zero
      | Update_returning { returning; _ } ->
        let query =
          Caqti_request.collect Caqti_type.unit (type_of_expr returning)
            query_str
        in
        collect query
    in
    res |> error

  let prepare
      (type a res arity)
      (arg : a Expression.Type.t)
      (f : (a, 'tag) Expression.t -> (res, arity) Model.Query.t) :
      transaction ->
      a ->
      ( (res, arity, ([> Engine.error ] as 'error)) Model.Query.result,
        'error )
      Lwt_result.t =
    let query = f (Expression.var arg)
    and arg = type_of_type arg in
    let query_str info =
      let q = Fmt.str "%a" (pp_query info) query in
      let () = Logs.debug (fun m -> m "caqti: prepare query: %s" q) in
      q
    in
    match Model.Query.arity query with
    | Zero ->
      let prepared =
        Caqti_request.create_p arg Caqti_type.unit Caqti_mult.zero query_str
      in
      fun (module C : Caqti_lwt.CONNECTION) v ->
        let+ () = C.exec prepared v |> error in
        Model.Query.Zero
    | One ->
      let prepared =
        Caqti_request.create_p arg (query_type query) Caqti_mult.one query_str
      in
      fun (module C : Caqti_lwt.CONNECTION) v ->
        let+ res = C.find prepared v |> error in
        Model.Query.One res
    | Many ->
      let prepared =
        Caqti_request.create_p arg (query_type query) Caqti_mult.zero_or_more
          query_str
      in
      fun (module C : Caqti_lwt.CONNECTION) v ->
        let+ res =
          let f response =
            let f stream =
              Lwt.bind (stream ()) ~f:(function
                | Caqti_lwt.Stream.Nil -> Lwt_result.return None
                | Error e -> Lwt_result.fail e |> error
                | Cons (v, next) -> Lwt_result.return @@ Some (v, next))
            and init = C.Response.to_stream response in
            Lwt_result.return @@ Sequence.unfold_step ~init ~f
          in
          C.call ~f prepared v |> error
        in
        Model.Query.Many res

  let connect { pool } f =
    let f c = f c |> Lwt_result.map_err (fun e -> `Shitty_api e)
    and unmap = function
      | `Shitty_api e -> e
      | #Caqti_error.t as e -> `Sqml_engine (Caqti_error.show e)
    in
    Caqti_lwt.Pool.use f
      (pool
        : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t
        :> (Caqti_lwt.connection, [> Caqti_error.t ]) Caqti_lwt.Pool.t)
    |> Lwt_result.map_err unmap
end
