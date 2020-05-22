open Acid
open Sqml
module Array = Array.Monadic2 (Lwt_result)
module Sequence = Engine.Sequence

open Let.Syntax2 (Lwt_result)

module Make (Model : Model.S) = struct
  type 'model t = {
    pool : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t;
  }

  let string_of_type : type t. t Sqml.Type.t -> string = function
    | Sqml.Type.Int -> "INTEGER"
    | Sqml.Type.Text -> "TEXT"
    | Sqml.Type.Timestamp -> "TEXT"
    | Sqml.Type.Varchar len -> Format.sprintf "VARCHAR(%i)" len

  let info f = Logs_lwt.info f |> Lwt.map ~f:Result.return

  let fail fmt =
    Caml.Format.ksprintf (fun s -> Lwt_result.fail (`Sqml_engine s)) fmt

  let convert_error =
    let f e = `Sqml_engine (Caqti_error.show e) in
    Result.map_error ~f

  let error = Lwt.map ~f:convert_error

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
          | _ -> failwith "dialect not handled"
        in
        let* found = C.find found_query () in
        let caqti_metadata_type = Caqti_type.(tup4 int int int ptime) in
        if found then
          let query =
            Caqti_request.find Caqti_type.unit caqti_metadata_type
              "SELECT major, minor, subminor, migrated FROM sqml_metadata \
               ORDER BY datetime(migrated) DESC LIMIT 1"
          in
          let* major, minor, subminor, _ = C.find query () in
          let current = (major, minor, subminor) in
          let* () =
            info (fun m ->
                m "load model at version %s" @@ Semver.to_string current)
          in
          Lwt_result.return (true, current)
        else
          let* () =
            info (fun m ->
                m "create model at version %s" @@ Semver.to_string Model.version)
          in
          let create_table (module M : Table.Meta) =
            let rec column : type t. t Sqml.Column.List.t -> string list =
              function
              | Sqml.Column.List.Cons ({ Sqml.Column.name; type_ }, tl) ->
                let col = Format.sprintf "%s %s" name (string_of_type type_) in
                col :: column tl
              | Sqml.Column.List.Empty -> []
            in
            let module M = M in
            let query =
              Caqti_request.exec ~oneshot:true Caqti_type.unit
                (Format.sprintf "CREATE TABLE %S (%s)" M.name
                   (String.concat ~sep:", " (column M.columns)))
            in
            C.exec query ()
          in
          let* () = Array.iter ~f:create_table (Model.tables ()) in
          let query =
            Caqti_request.exec caqti_metadata_type
              "INSERT INTO sqml_metadata (major, minor, subminor, migrated) \
               VALUES (?, ?, ?, ?)"
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
      if current <> Model.version then
        fail "model version mismatch: code at %s, data at %s"
          (Semver.to_string Model.version)
          (Semver.to_string current)
      else
        Lwt_result.return { pool }
    else
      fail "backend does not support transactions"

  module Model = Model

  let debug f = Logs_lwt.debug f |> Lwt.map ~f:Result.return

  let info f = Logs_lwt.info f |> Lwt.map ~f:Result.return

  let fail fmt =
    Caml.Format.ksprintf (fun s -> Lwt_result.fail (`Sqml_engine s)) fmt

  let type_of_column : type t. t Sqml.Column.t -> t Caqti_type.t = function
    | { Column.type_; _ } -> (
      match type_ with
      | Sqml.Type.Int -> Caqti_type.int
      | Sqml.Type.Text -> Caqti_type.string
      | Sqml.Type.Timestamp -> Caqti_type.ptime
      | Sqml.Type.Varchar _ -> Caqti_type.string )

  let rec type_of_s_list : type t. t Column.List.t -> t Caqti_type.t = function
    | Column.List.Empty -> Caqti_type.unit
    | Cons (c, tl) -> Caqti_type.tup2 (type_of_column c) (type_of_s_list tl)

  let rec type_of_expr : type t. (t, _) Priv.Expression.t -> t Caqti_type.t =
    function
    | Priv.Expression.Column { column; _ } -> type_of_column column
    | Priv.Expression.Pair (a, b) ->
      Caqti_type.tup2 (type_of_expr a) (type_of_expr b)

  let insert
      (type mapped)
      { pool }
      (module Table : Table.S with type Meta.mapped = mapped)
      (mapped : mapped) =
    let module T = Table in
    let query =
      let t =
        let ( % ) = Fn.compose in
        let encode = Result.return % T.Meta.unmap
        and decode = Result.return % T.Meta.map in
        Caqti_type.custom ~encode ~decode (type_of_s_list T.Meta.columns)
      in
      Caqti_request.exec t
      @@ Format.sprintf "INSERT INTO %S VALUES (%s)" T.name
           (String.concat ~sep:", "
              (List.init ~f:(fun _ -> "?") (Column.List.length T.Meta.columns)))
    in
    let insert value (module C : Caqti_lwt.CONNECTION) = C.exec query value in
    Caqti_lwt.Pool.use (insert mapped) pool |> error

  let query { pool } query =
    let query_str = Fmt.str "%a" Model.Query.pp query in
    let* () = debug @@ fun m -> m "caqti: execute query: %s" query_str in
    match query with
    | Select (e, s) ->
      let module Selected = (val s) in
      let query =
        Caqti_request.collect Caqti_type.unit (type_of_expr e) query_str
      in
      let get (module C : Caqti_lwt.CONNECTION) =
        let f response =
          let f
              (stream :
                ('c, ([> Caqti_error.retrieve ] as 'err)) Caqti_lwt.Stream.t) =
            Lwt.bind (stream ()) ~f:(function
              | Caqti_lwt.Stream.Nil -> Lwt_result.return None
              | Error e -> Lwt_result.fail e |> error
              | Cons (v, next) -> Lwt_result.return @@ Some (v, next))
          and init = C.Response.to_stream response in
          Lwt_result.return @@ Sequence.unfold_step ~init ~f
        in
        C.call ~f query ()
      in
      Caqti_lwt.Pool.use get pool |> error
end
