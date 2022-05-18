open Sqml
open Utils
module Array = Array.Monadic2 (Lwt_result)
open Let.Syntax2 (Lwt_result)
open Let.Syntax' (Lwt)

module Make (Model : Model.S) : sig
  include Engine.S with module Model = Model

  val make :
    ?max_size:int -> string -> (t, [> `Sqml_engine of string ]) Lwt_result.t
end = struct
  type t = { pool : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t }
  type transaction = (module Caqti_lwt.CONNECTION) Lwt_guarded.t

  let quote_pattern = String.Search_pattern.create "'"

  let pp_literal_string f value =
    let open Fmt in
    let fmt = const char '\'' ++ string ++ const char '\'' in
    fmt f
      (String.Search_pattern.replace_all quote_pattern ~in_:value ~with_:"''")

  let rec pp_from_type : type t. t Sqml.Expression.Type.t -> t Fmt.t =
    let open Fmt in
    let pp_time fmt t =
      char fmt '\'';
      string fmt (Timmy.Time.to_rfc3339 t);
      char fmt '\''
    in
    let rec pp_values :
        type l. l Hlist.t Sqml.Expression.Type.t -> l Hlist.t Fmt.t = function
      | [] -> nop
      | [ hd ] -> fun f [ hd_v ] -> pp_from_type hd f hd_v
      | hd :: tl ->
        fun f (hd_v :: tl_v) ->
          pp_from_type hd f hd_v;
          comma f ();
          pp_values tl f tl_v
      | Custom { repr; encode; _ } -> fun f v -> pp_from_type repr f @@ encode v
      | Time ->
        (* Timmy.Time.t being abstract, the typer cannot prove it's not
           unifiable with [l Hlist.t], so we must handle it. *)
        pp_time
    in
    function
    | Bigint -> Fmt.int64
    | Bool -> Fmt.bool
    | Date -> const char '\'' ++ Timmy.Date.pp ++ const char '\''
    | Float -> Fmt.float
    | Int -> Fmt.int
    | Json -> using Ezjsonm.(value_to_string ~minify:true) pp_literal_string
    | Null -> Fmt.(const string "NULL")
    | Nullable t -> Fmt.(option ~none:(const string "NULL") @@ pp_from_type t)
    | String -> pp_literal_string
    | Time -> pp_time
    | Custom { encode; repr; _ } ->
      fun f value -> pp_from_type repr f @@ encode value
    | [] -> nop
    | _ :: _ as l -> pp_values l

  and pp_expression =
    let open Fmt in
    let var = Fmt.(const char '$' ++ using Int.succ int) in
    let rec pp : Expression.ast Fmt.t =
     fun f -> function
      | Column { selectable; name; _ } ->
        (const string selectable ++ const string "." ++ const string name) f ()
      | Const { type_; value } -> pp_from_type type_ f value
      | Funcall { name; args; _ } ->
        (const string name ++ sp ++ const char '(' ++ list ~sep:comma pp
       ++ const char ')')
          f args
      | Infix { op; lhs; rhs } ->
        let fmt = const pp lhs ++ sp ++ const string op ++ sp ++ const pp rhs in
        fmt f ()
      | Is_null ast -> (pp ++ sp ++ const string "IS NULL") f ast
      | Is_not_null ast -> (pp ++ sp ++ const string "IS NOT NULL") f ast
      | List l -> list ~sep:comma pp f l
      | Null -> Fmt.string f "NULL"
      | Switch { cases; default } ->
        let pp_case f (cond, value) =
          (const string "WHEN" ++ sp ++ const pp cond ++ sp
         ++ const string "THEN" ++ sp ++ const pp value ++ sp)
            f ()
        in
        (const string "CASE" ++ sp
        ++ const (list pp_case) cases
        ++ const (option (const string "ELSE" ++ sp ++ pp ++ sp)) default
        ++ const string "END")
          f ()
      | Var { index; size } ->
        (list ~sep:comma var) f @@ List.range index (index + size)
    in
    pp

  let table_name info schema name =
    match schema with
    | None -> Fmt.str "%S" name
    | Some schema -> (
      match Caqti_driver_info.dialect_tag info with
      | `Sqlite -> Fmt.str "%s_%s" (String.escaped schema) (String.escaped name)
      | `Pgsql -> Fmt.str "%S.%S" schema name
      | _ -> failwith "dialect not handled")

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
        match f.f column with Some value -> value :: map tl | None -> map tl)
      | [] -> []
    in
    map

  let primary columns =
    let f { Column.name; primary; _ } = if primary then Some name else None in
    column_map { f } columns

  let make ?max_size uri =
    let open Caqti_request.Infix in
    let* pool =
      Caqti_lwt.connect_pool ?max_size (Uri.of_string uri)
      |> Lwt.return |> error
    in
    let create (module C : Caqti_lwt.CONNECTION) =
      let i = C.driver_info in
      if not @@ Caqti_driver_info.can_transact i then Lwt_result.return None
      else
        let dialect, found_query =
          match Caqti_driver_info.dialect_tag i with
          | `Sqlite ->
            ( Sqml.Dialect.sqlite,
              (Caqti_type.unit ->! Caqti_type.bool)
                "SELECT COUNT(*) > 0 FROM sqlite_master WHERE type='table' AND \
                 name= 'sqml_metadata'" )
          | `Pgsql ->
            ( Sqml.Dialect.postgresql,
              (Caqti_type.unit ->! Caqti_type.bool)
                "SELECT EXISTS (SELECT FROM information_schema.tables WHERE \
                 table_schema = 'sqml' AND table_name = 'metadata')" )
          | _ -> failwith "dialect not handled"
        in
        let* found = C.find found_query () in
        let caqti_metadata_type = Caqti_type.(tup4 int int int ptime) in
        if found then
          let query =
            (Caqti_type.unit ->! caqti_metadata_type)
              (Fmt.str
                 "SELECT major, minor, subminor, migrated FROM %s ORDER BY %s \
                  DESC LIMIT 1"
                 (table_name i (Some "sqml") "metadata")
                 (if Poly.(Caqti_driver_info.dialect_tag i = `Sqlite) then
                  "datetime(migrated)"
                 else "migrated"))
          in
          let* major, minor, subminor, _ = C.find query () in
          let current = Semver.from_parts major minor subminor [] [] in
          let* () =
            Log.info (fun m ->
                m "load model at version %a" Fmt.(option Semver.pp) current)
          in
          Lwt_result.return current
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
                    (Caqti_type.unit ->. Caqti_type.unit)
                      ~oneshot:true
                      (Fmt.str "CREATE SCHEMA IF NOT EXISTS %S" schema)
                  in
                  C.exec query ())
              | _ -> failwith "dialect not handled"
            in
            let queries = Sqml.Model.table_schema dialect (module T) in
            let* () =
              Log.debug (fun m ->
                  m "create table: %a"
                    Fmt.(list ~sep:(const char ';' ++ sp) string)
                    queries)
            in
            let exec query =
              let query =
                (Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true query
              in
              C.exec query ()
            in
            Lwt_list.map_s exec queries |> Lwt.map ~f:Result.all_unit
          in
          let* () = Array.iter ~f:create_table (Model.tables ()) in
          let query =
            (caqti_metadata_type ->. Caqti_type.unit)
              (Fmt.str
                 "INSERT INTO %s (major, minor, subminor, migrated) VALUES (?, \
                  ?, ?, ?)"
                 (table_name i (Some "sqml") "metadata"))
          in
          let* () =
            let Semver.{ major; minor; patch; _ } = Model.version
            and now = Option.value_exn (Ptime.of_float_s @@ Unix.time ()) in
            C.exec query (major, minor, patch, now)
          in
          Lwt_result.return (Some Model.version)
    in
    Caqti_lwt.Pool.use create pool |> error >>= function
    | Some current ->
      if Poly.(current <> Model.version) then
        fail "model version mismatch: code at %s, data at %s"
          (Semver.to_string Model.version)
          (Semver.to_string current)
      else Lwt_result.return { pool }
    | None -> fail "backend does not support transactions"

  let close { pool } = Caqti_lwt.Pool.drain pool

  module Model = Model

  let rec type_of_type : type t. t Sqml.Expression.Type.t -> t Caqti_type.t =
    function
    | Bigint -> Caqti_type.int64
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
    | Float -> Caqti_type.float
    | Int -> Caqti_type.int
    | Json ->
      let encode json =
        Result.return (Ezjsonm.value_to_string ~minify:true json)
      and decode string =
        try Result.return (Ezjsonm.value_from_string string)
        with Ezjsonm.Parse_error (_, error) -> Result.fail error
      in
      Caqti_type.(custom ~encode ~decode string)
    | Null -> failwith "FIXME"
    | Nullable t -> Caqti_type.option (type_of_type t)
    | String -> Caqti_type.string
    | Time ->
      let encode t = Result.return @@ Timmy.Time.to_ptime t
      and decode t = Result.return @@ Timmy.Time.of_ptime t in
      Caqti_type.custom ~decode ~encode @@ Caqti_type.ptime
    | Custom { encode; decode; repr } ->
      Caqti_type.custom ~decode ~encode:(fun v -> Result.return @@ encode v)
      @@ type_of_type repr
    | [] ->
      let encode Hlist.[] = Result.return ()
      and decode () = Result.return Hlist.[] in
      Caqti_type.custom ~decode ~encode @@ Caqti_type.unit
    | l :: r ->
      let encode Hlist.(hd :: tl) = Result.return @@ (hd, tl)
      and decode (hd, tl) = Result.return @@ Hlist.(hd :: tl) in
      Caqti_type.custom ~decode ~encode
      @@ Caqti_type.tup2 (type_of_type l) (type_of_type r)

  let rec type_of_types :
      type l. l Sqml.Expression.Type.List.t -> l Caqti_type.t = function
    | hd :: tl -> Caqti_type.tup2 (type_of_type hd) (type_of_types tl)
    | [] -> Caqti_type.unit

  let type_of_expr e = Sqml.Expression.to_type e |> type_of_type

  let query_type : type t arity. (t, arity) Model.Query.t -> t Caqti_type.t =
    function
    | Delete _ -> Caqti_type.unit
    | Delete_returning { returning; _ } -> type_of_expr returning
    | Insert _ -> Caqti_type.unit
    | Insert_returning { returning; _ } -> type_of_expr returning
    | Select { what; _ } -> type_of_expr what
    | Update _ -> Caqti_type.unit
    | Update_returning { returning; _ } -> type_of_expr returning

  let rec pp_selectable f =
    let open Fmt in
    function
    | Selectable.Selectable { name; alias } ->
      (const string name ++ const (option (sp ++ string)) alias) f ()
    | Join { left; right; on; type_ } ->
      let verb =
        match type_ with
        | Inner -> "INNER JOIN"
        | Outer -> "OUTER JOIN"
        | Outer_left -> "LEFT OUTER JOIN"
        | Outer_right -> "RIGHT OUTER JOIN"
      in
      (const pp_selectable left ++ sp ++ const string verb ++ sp
     ++ const pp_selectable right ++ sp ++ const string "ON" ++ sp
      ++ const (option ~none:(const string "true") pp_expression) on)
        f ()

  let dialect_of_dialect_tag = function
    | #Caqti_driver_info.sql_dialect_tag as t -> t
    | _ -> `Other

  let pp_query dialect =
    let pp : type tag arity. (tag, arity) Model.Query.t Fmt.t =
     fun f ->
      let open Fmt in
      let pp_assignment f assignment =
        let name, ast = Expression.assignment_to_ast assignment in
        Fmt.(const string name ++ sp ++ const char '=' ++ sp ++ pp_expression)
          f ast
      and pp_returning : type t tag. (t, tag) Expression.option Fmt.t =
       fun f -> function
        | Expression.None -> ()
        | Some e ->
          Fmt.(
            (sp ++ const string "RETURNING" ++ sp ++ box pp_expression)
              f (Expression.to_ast e))
      in
      let pp_insert (type mapped res tag)
          (module T : Table.S
            with type model = Model.tag
             and type tag = tag
             and type Meta.mapped = mapped)
          ({ excluded } : tag Sqml.Model.excluded)
          (on_conflict : tag Sqml.Model.on_conflict option)
          (returning : (res, tag) Expression.option) value =
        let conflict_sqlite, conflict_pg =
          let open Fmt in
          match (dialect, on_conflict) with
          | _, None -> (nop, nop)
          | (`Pgsql | `Other), Some Ignore ->
            (nop, const string " ON CONFLICT DO NOTHING")
          | (`Pgsql | `Other), Some (Set assignments) ->
            ( nop,
              const string " ON CONFLICT ("
              ++ const (list ~sep:comma string) (primary T.Meta.columns)
              ++ const string ") DO UPDATE SET "
              ++ const
                   (list ~sep:comma pp_assignment)
                   (assignments { excluded }) )
          | (`Pgsql | `Other), Some Update ->
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
          | `Sqlite, Some (Set _) | `Mysql, _ -> failwith "dialect not handled"
        in
        (* Explicitely enumerate columns order on insertion, migration that add
           columns subsequently mess up the column ordering. *)
        Fmt.(
          pf f "INSERT%a INTO %S (%a) VALUES(%a)%a%a" conflict_sqlite ()
            T.Meta.name (list ~sep:comma string)
            (Table.Column.List.map
               ~f:{ f = (fun { name; _ } -> name) }
               T.Meta.columns)
            pp_expression value conflict_pg () pp_returning returning)
      and pp_update (type res tag from)
          (module T : Table.S with type model = Model.tag and type tag = tag)
          (what : tag Expression.assignment list)
          (from :
            (module Selectable.S with type model = Model.tag and type tag = from)
            option) (where : (bool, tag) Expression.t option)
          (returning : (res, tag) Expression.option) =
        let from =
          match from with
          | None -> None
          | Some (module From) -> Some (From.to_ast ())
        in
        (box
        @@ const string "UPDATE" ++ sp
           ++ const pp_selectable (T.to_ast ())
           ++ sp ++ const string "SET" ++ sp
           ++ const (list ~sep:comma pp_assignment) what
           ++ const
                (option (sp ++ const string "FROM" ++ sp ++ pp_selectable))
                from
           ++ const
                (option (sp ++ const string "WHERE" ++ sp ++ pp_expression))
                Expression.(Option.map ~f:to_ast where)
           ++ const pp_returning returning)
          f ()
      in
      let pp_clause keyword f e = Fmt.pf f "@ %s %a" keyword pp_expression e in
      function
      | Delete { from; where } ->
        let module Table = (val from) in
        Fmt.pf f "DELETE FROM@ %a%a" pp_selectable (Table.to_ast ())
          (option (pp_clause "WHERE"))
          Expression.(Option.map ~f:to_ast where)
      | Delete_returning { from; returning; where } ->
        let module Table = (val from) in
        Fmt.pf f "DELETE FROM@ %a%a%a" pp_selectable (Table.to_ast ())
          (option (pp_clause "WHERE"))
          Expression.(Option.map ~f:to_ast where)
          (pp_clause "RETURNING")
          Expression.(to_ast returning)
      | Insert { excluded; into; on_conflict; value } ->
        pp_insert into { excluded } on_conflict Expression.None
          Expression.(to_ast value)
      | Insert_returning { excluded; into; on_conflict; returning; value } ->
        pp_insert into { excluded } on_conflict (Some returning)
          Expression.(to_ast value)
      | Select { for_; from; what; where } ->
        let pp_for f =
          let forable =
            let rec forable =
              let outer (name, _) = (name, false) in
              function
              | Selectable.Selectable { alias; name } ->
                [ (Option.value ~default:name alias, true) ]
              | Selectable.Join { left; right; type_ = Inner; _ } ->
                forable left @ forable right
              | Selectable.Join { left; right; type_ = Outer_left; _ } ->
                forable left @ List.map ~f:outer @@ forable right
              | Selectable.Join { left; right; type_ = Outer_right; _ } ->
                (List.map ~f:outer @@ forable left) @ forable right
              | Selectable.Join { left; right; type_ = Outer; _ } ->
                (List.map ~f:outer @@ forable left)
                @ List.map ~f:outer @@ forable right
            in
            let module From = (val from) in
            forable (From.to_ast ())
          and pp_forable f forable =
            if List.for_all ~f:snd forable then ()
            else
              Fmt.pf f " OF %a" (list ~sep:comma string)
              @@ List.filter_map
                   ~f:(fun (name, forable) -> Option.some_if forable name)
                   forable
          in
          function
          | Sqml.Model.Share ->
            Fmt.(const string "FOR SHARE" ++ pp_forable) f forable
          | Update -> Fmt.(const string "FOR UPDATE" ++ pp_forable) f forable
        in
        let module Selectable = (val from) in
        (box
        @@ const string "SELECT" ++ sp
           ++ const pp_expression Expression.(to_ast what)
           ++ sp ++ const string "FROM" ++ sp
           ++ const pp_selectable (Selectable.to_ast ())
           ++ const
                (option (pp_clause "WHERE"))
                Expression.(Option.map ~f:to_ast where)
           ++ const (option (sp ++ pp_for)) for_)
          f ()
      | Update { into; what; where } ->
        pp_update into what None where Expression.None
      | Update_returning { from; into; returning; where; what } ->
        pp_update into what from where Expression.(Some returning)
    in
    pp

  let query transaction (type res arity) (query : (res, arity) Model.Query.t) :
      ((res, arity, _) Sqml.Model.result, _) Lwt_result.t =
    let open Caqti_request.Infix in
    let f (module C : Caqti_lwt.CONNECTION) =
      let collect query =
        let f response =
          let f stream =
            Lwt.bind (stream ()) ~f:(function
              | Caqti_lwt.Stream.Nil -> Lwt_result.return None
              | Error e -> Lwt_result.fail e |> error
              | Cons (v, next) -> Lwt_result.return @@ Some (v, next))
          and init = C.Response.to_stream response in
          Lwt_result.return @@ Sqml.Model.Many (Sequence.unfold_step ~init ~f)
        in
        C.call ~f query ()
      in
      let query_str =
        Fmt.str "%a"
          (pp_query @@ dialect_of_dialect_tag
          @@ Caqti_driver_info.dialect_tag C.driver_info)
          query
      in
      let* () = Log.debug @@ fun m -> m "caqti: execute query: %s" query_str in
      let res : ((res, arity, _) Sqml.Model.result, _) Lwt_result.t =
        match query with
        | Delete _ ->
          let query = (Caqti_type.unit ->. Caqti_type.unit) query_str in
          let+ () = C.exec query () in
          Sqml.Model.Zero
        | Delete_returning { returning; _ } ->
          let query = (Caqti_type.unit ->* type_of_expr returning) query_str in
          collect query
        | Select { what; _ } ->
          let query = (Caqti_type.unit ->* type_of_expr what) query_str in
          collect query
        | Insert _ ->
          let query = (Caqti_type.unit ->. Caqti_type.unit) query_str in
          let+ () = C.exec query () in
          Sqml.Model.Zero
        | Insert_returning { returning; _ } ->
          let query = (Caqti_type.unit ->! type_of_expr returning) query_str in
          let+ res = C.find query () in
          Sqml.Model.One res
        | Update _ ->
          let query = (Caqti_type.unit ->. Caqti_type.unit) query_str in
          let+ () = C.exec query () in
          Sqml.Model.Zero
        | Update_returning { returning; _ } ->
          let query = (Caqti_type.unit ->* type_of_expr returning) query_str in
          collect query
      in
      res |> error
    in
    Lwt_guarded.acquire ~f transaction

  module Hlist_printers = Hlist.Make (struct
    type 'a t = 'a Fmt.t
  end)

  module Printers_of_types = Sqml.Expression.Type.List.Transduce (Hlist_printers)

  let pp_values_of_types types =
    let open Fmt in
    let printers = Printers_of_types.map ~f:{ f = pp_from_type } types in
    let rec print :
        type l. l Hlist_printers.t -> Formatter.t -> l Hlist.t -> unit =
      function
      | [] -> fun _ [] -> ()
      | [ printer ] -> fun fmt [ value ] -> printer fmt value
      | printer :: printers ->
        let tail = print printers in
        fun fmt (value :: values) ->
          printer fmt value;
          Fmt.comma fmt ();
          tail fmt values
    in
    const string "[" ++ sp ++ print printers ++ sp ++ const string "]"

  type ('args, 'tag, 'res, 'arity) prepared = {
    query : ('res, 'arity) Model.Query.t;
    prepared : ('args, 'res, 'arity) prepared_query;
    types : 'args Expression.Type.List.t;
  }

  and ('args, 'res, 'arity) prepared_query =
    | Zero :
        ('args, unit, [ `Zero ]) Caqti_request.t
        -> ('args, 'res, Sqml.Model.zero) prepared_query
    | One :
        ('args, 'res, [ `One ]) Caqti_request.t
        -> ('args, 'res, Sqml.Model.one) prepared_query
    | Many :
        ('args, 'res, [ `Many | `One | `Zero ]) Caqti_request.t
        -> ('args, 'res, Sqml.Model.many) prepared_query

  let prepare (type args res arity) (types : args Expression.Type.List.t)
      (query : (args, 'tag) Expression.List.t -> (res, arity) Model.Query.t) :
      (args, _, res, arity) prepared =
    let query = query @@ Sqml.Expression.prepare types
    and args = type_of_types types in
    let prepare_str tag =
      let query_str dialect =
        let q = Fmt.str "%a" (pp_query dialect) query in
        let () = Logs.debug (fun m -> m "caqti: prepare query: %s" q)
        and () =
          Expression.Type.List.iter
            ~f:
              {
                f =
                  (fun t ->
                    Logs.debug (fun m ->
                        m "caqti: arguments: %a" Sqml.Expression.Type.pp t));
              }
            types
        in
        q
      in
      query_str (dialect_of_dialect_tag tag)
    in
    let open Caqti_request.Infix in
    match Model.Query.arity query with
    | Zero ->
      let prepared = (args -->. Caqti_type.unit) @@:- prepare_str in
      { query; prepared = Zero prepared; types }
    | One ->
      let prepared = (args -->! query_type query) @@:- prepare_str in
      { query; prepared = One prepared; types }
    | Many ->
      let prepared = (args -->* query_type query) @@:- prepare_str in
      { query; prepared = Many prepared; types }

  let run (type args res arity)
      ({ query; prepared; types } : (args, 'tag, res, arity) prepared)
      transaction (values : args Hlist.t) =
    let* () =
      Log.debug (fun m ->
          m "caqti: execute prepared query: %a" (pp_query `Pgsql) query)
    and* () =
      let pp_values = pp_values_of_types types in
      Log.debug (fun m -> m "caqti: values: %a" pp_values values)
    in
    match prepared with
    | Zero prepared ->
      let+ () =
        let f (module C : Caqti_lwt.CONNECTION) =
          C.exec prepared (Hlist.to_pairs values) |> error
        in
        Lwt_guarded.acquire ~f transaction
      in
      (Sqml.Model.Zero
        : (res, arity, ([> Sqml.Engine.error ] as 'error)) Sqml.Model.result)
    | One prepared ->
      let+ res =
        let f (module C : Caqti_lwt.CONNECTION) =
          C.find prepared (Hlist.to_pairs values) |> error
        in
        Lwt_guarded.acquire ~f transaction
      in
      Sqml.Model.One res
    | Many prepared ->
      let+ res =
        let f (module C : Caqti_lwt.CONNECTION) =
          let f response =
            let f stream =
              Lwt.bind (stream ()) ~f:(function
                | Caqti_lwt.Stream.Nil -> Lwt_result.return None
                | Error e -> Lwt_result.fail e |> error
                | Cons (v, next) -> Lwt_result.return @@ Some (v, next))
            and init = C.Response.to_stream response in
            Lwt_result.return @@ Sequence.unfold_step ~init ~f
          in
          C.call ~f prepared (Hlist.to_pairs values) |> error
        in
        Lwt_guarded.acquire ~f transaction
      in
      Sqml.Model.Many res

  let connect { pool } f =
    let f c =
      let module C = (val c : Caqti_lwt.CONNECTION) in
      let* () = C.start () in
      let f () =
        f (Lwt_guarded.make c) >>=: function
        | Result.Error e ->
          let* () = C.rollback () in
          Lwt_result.fail (`Shitty_api e)
        | Result.Ok res ->
          let+ () = C.commit () in
          res
      and catch exn =
        let* () = C.rollback () in
        raise exn
      in
      Lwt.catch f catch
    and unmap = function
      | `Shitty_api e -> e
      | #Caqti_error.t as e -> `Sqml_engine (Caqti_error.show e)
    in
    Caqti_lwt.Pool.use f
      (pool
        : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t
        :> (Caqti_lwt.connection, [> Caqti_error.t ]) Caqti_lwt.Pool.t)
    |> Lwt_result.map_err unmap

  let schema t =
    let type_of_info column char_length nullable data_type =
      let open Let.Syntax2 (Result) in
      let nullable (Column.Type.Type t as type_) =
        if String.(nullable = "YES") then Column.Type.Type (Nullable t)
        else type_
      in
      let+ type_ =
        match (data_type, char_length) with
        | "boolean", _ -> Result.return (Column.Type.Type Bool)
        | "character varying", Some l ->
          Result.return (Column.Type.Type (Varchar l))
        | "character varying", None ->
          Result.fail
            (`Sqml_engine
              (Fmt.str
                 "missing character_maximum_length in information_schema for \
                  column %s"
                 column))
        | "date", _ -> Result.return (Column.Type.Type Date)
        | "integer", _ -> Result.return (Column.Type.Type Int)
        | "jsonb", _ -> Result.return (Column.Type.Type Json)
        | "text", _ -> Result.return (Column.Type.Type Text)
        | ("timestamp" | "timestamp without time zone"), _ ->
          Result.return (Column.Type.Type Time)
        | _ ->
          Result.fail
            (`Sqml_engine
              (Fmt.str "unrecognized data_type in information_schema: %S"
                 data_type))
      in
      nullable type_
    in
    let f (module C : Caqti_lwt.CONNECTION) =
      let open Caqti_request.Infix in
      let* tables =
        let query =
          (Caqti_type.unit ->* Caqti_type.(tup2 string string))
            ~oneshot:true
            {| SELECT schemaname, tablename FROM pg_catalog.pg_tables
               WHERE schemaname != 'pg_catalog'
                 AND schemaname != 'information_schema' |}
        in
        C.collect_list query () |> error
      in
      let f (schema, name) =
        let* columns =
          let query =
            (Caqti_type.(tup2 string string)
            ->* Caqti_type.(tup4 (option int) string string string))
              {| SELECT character_maximum_length,
                        column_name,
                        data_type,
                        is_nullable
                 FROM information_schema.columns
                 WHERE table_schema = $1
                   AND table_name   = $2 |}
          in
          C.collect_list query (schema, name) |> error
        in
        let+ columns =
          let f (char_length, name, data_type, nullable) =
            let open Let.Syntax2 (Result) in
            let+ type_ = type_of_info name char_length nullable data_type in
            { Table.Schema.name; type_ }
          in
          columns |> List.map ~f |> Result.all |> Lwt.return
        in
        let schema = match schema with "public" -> None | _ -> Some schema in
        {
          Sqml.Table.Schema.columns =
            Set.of_list (module Table.Schema.Column_by_name) columns;
          name;
          schema;
        }
      in
      Sequence.of_list tables |> Sequence.Seq.map ~f |> Sequence.to_list
    in
    connect t (fun transaction -> Lwt_guarded.acquire ~f transaction)
end
