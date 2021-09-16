open Acid
open Sqml
open Utils
module Array = Array.Monadic2 (Lwt_result)

open Let.Syntax2 (Lwt_result)

module Make (Model : Model.S) = struct
  type 'model t = {
    pool : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t;
  }

  let table_name info schema name =
    match schema with
    | None -> Format.sprintf "%S" name
    | Some schema -> (
      match Caqti_driver_info.dialect_tag info with
      | `Sqlite ->
        Format.sprintf "%s_%s" (String.escaped schema) (String.escaped name)
      | `Pgsql -> Format.sprintf "%S.%S" schema name
      | _ -> failwith "dialect not handled")

  let nullable : type t. t Type.t -> bool = function
    | Type.Nullable _ -> true
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

  let rec type_of_type : type t. t Sqml.Type.t -> t Caqti_type.t = function
    | Sqml.Type.Bool -> Caqti_type.bool
    | Sqml.Type.Custom { encode; decode; repr } ->
      Caqti_type.custom ~decode ~encode @@ type_of_type repr
    | Sqml.Type.Int -> Caqti_type.int
    | Sqml.Type.Nullable t -> Caqti_type.option (type_of_type t)
    | Sqml.Type.Text -> Caqti_type.string
    | Sqml.Type.Time ->
      let encode t = Result.return @@ Timmy.Time.to_ptime t
      and decode t = Result.return @@ Timmy.Time.of_ptime t in
      Caqti_type.custom ~decode ~encode @@ Caqti_type.ptime
    | Sqml.Type.Varchar _ -> Caqti_type.string

  let rec type_of_expr : type t. (t, _) Expression.t -> t Caqti_type.t =
    function
    | Expression.Column { type_; _ } -> type_of_type type_
    | Expression.Comparison _ -> Caqti_type.bool
    | Expression.Int _ -> Caqti_type.int
    | Expression.Logic _ -> Caqti_type.bool
    | Expression.Map { mapped; encode; decode } ->
      Caqti_type.custom
        ~decode:(fun x -> Result.Ok (decode x))
        ~encode:(fun x -> Result.Ok (encode x))
      @@ type_of_expr mapped
    | Expression.Pair (a, b) ->
      Caqti_type.tup2 (type_of_expr a) (type_of_expr b)
    | Expression.String _ -> Caqti_type.string
    | Expression.Unit -> Caqti_type.unit
    | Expression.Var -> failwith "prepared query of variable type not supported"

  let rec type_of_expressions :
      type t tag. (t, tag) Expression.List.t -> t Caqti_type.t = function
    | Expression.List.[] -> Caqti_type.unit
    | c :: tl -> Caqti_type.tup2 (type_of_expr c) (type_of_expressions tl)

  let query_type : type t arity. (t, arity) Model.Query.t -> t Caqti_type.t =
    function
    | Model.Query.Delete _ -> Caqti_type.unit
    | Delete_returning { returning; _ } -> type_of_expr returning
    | Insert _ -> Caqti_type.unit
    | Select { what; _ } -> type_of_expr what

  let query { pool } query =
    let query_str = Fmt.str "%a" Model.Query.pp query in
    let* () = Log.debug @@ fun m -> m "caqti: execute query: %s" query_str in
    let collect query =
      let get (module C : Caqti_lwt.CONNECTION) =
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
      Caqti_lwt.Pool.use get pool |> error
    in
    let exec :
        type res arity.
        (res, arity) Model.Query.t ->
        ( (res, arity, 'error) Model.Query.result,
          [> `Sqml_engine of string ] )
        Lwt_result.t = function
      | Delete _ ->
        let query = Caqti_request.exec Caqti_type.unit query_str in
        let delete (module C : Caqti_lwt.CONNECTION) = C.exec query () in
        let+ () = Caqti_lwt.Pool.use delete pool |> error in
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
      | Insert { into; on_conflict; value } ->
        let module T = (val into) in
        let insert value (module C : Caqti_lwt.CONNECTION) =
          let i = C.driver_info in
          let conflict_sqlite, conflict_pg =
            match (Caqti_driver_info.dialect_tag i, on_conflict) with
            | _, None -> ("", "")
            | `Pgsql, Some Ignore -> ("", " ON CONFLICT DO NOTHING")
            | `Pgsql, Some Update ->
              let f { Column.name; _ } =
                Option.return @@ Fmt.str "%s = excluded.%s" name name
              in
              ( "",
                Fmt.str " ON CONFLICT (%s) DO UPDATE SET %s"
                  (String.concat ~sep:", " (primary T.Meta.columns))
                  (String.concat ~sep:", " (column_map { f } T.Meta.columns)) )
            | `Sqlite, Some Ignore -> (" OR IGNORE", "")
            | `Sqlite, Some Update -> (" OR REPLACE", "")
            | _ -> failwith "dialect not handled"
          in
          let* query =
            let t =
              let ( % ) = Fn.compose in
              let encode = Result.return % T.Meta.unmap
              and decode = Result.return % T.Meta.map in
              Caqti_type.custom ~encode ~decode
                (type_of_expressions T.Expression.columns)
            in
            let q =
              Fmt.str "INSERT%s INTO %S VALUES (%s)%s" conflict_sqlite
                T.Meta.name
                (String.concat ~sep:", "
                   (List.init
                      ~f:(fun _ -> "?")
                      (Expression.List.length T.Expression.columns)))
                conflict_pg
            in
            let+ () = Log.debug (fun m -> m "caqti: execute insertion: %s" q) in
            Caqti_request.exec t q
          in
          C.exec query value
        in
        let* () = Caqti_lwt.Pool.use (insert value) pool |> error in
        Lwt_result.return Model.Query.Zero
    in
    exec query

  module Prepare = struct
    type 't arg = 't Caqti_type.t

    let int = Caqti_type.int

    let string = Caqti_type.string

    let arg
        (type a res arity)
        (arg : a arg)
        (f : (a, 'tag) Expression.t -> (res, arity) Model.Query.t) :
        Model.tag t ->
        a ->
        ( (res, arity, ([> Engine.error ] as 'error)) Model.Query.result,
          'error )
        Lwt_result.t =
      let query = f (Expression.var ()) in
      let query_str = Fmt.str "%a" Model.Query.pp query in
      match Model.Query.arity query with
      | Zero ->
        let prepared = Caqti_request.exec arg query_str in
        let f v (module C : Caqti_lwt.CONNECTION) = C.exec prepared v in
        fun { pool } v ->
          let+ () = Caqti_lwt.Pool.use (f v) pool |> error in
          Model.Query.Zero
      | Many ->
        let prepared = Caqti_request.find arg (query_type query) query_str in
        let f v (module C : Caqti_lwt.CONNECTION) =
          let f response =
            let f stream =
              Lwt.bind (stream ()) ~f:(function
                | Caqti_lwt.Stream.Nil -> Lwt_result.return None
                | Error e -> Lwt_result.fail e |> error
                | Cons (v, next) -> Lwt_result.return @@ Some (v, next))
            and init = C.Response.to_stream response in
            Lwt_result.return @@ Sequence.unfold_step ~init ~f
          in
          C.call ~f prepared v
        in
        fun { pool } v ->
          let+ res = Caqti_lwt.Pool.use (f v) pool |> error in
          Model.Query.Many res
  end
end
