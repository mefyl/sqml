open Acid
open Sqml
module Sequence = Engine.Sequence

module Make (Model : Model.S) = struct
  type 'model t = { db : Sqlite3.db }

  module Model = Model

  open Acid.Let.Let2 (Lwt_result)

  let debug f = Logs_lwt.debug f |> Lwt.map ~f:Result.return

  let fail fmt =
    Caml.Format.ksprintf (fun s -> Lwt_result.fail (`Sqml_engine s)) fmt

  let fail_db db _ = fail "%s" (Sqlite3.errmsg db)

  let stmt db q =
    match Sqlite3.prepare db q with
    | stmt -> Lwt_result.return stmt
    | exception Sqlite3.Error e -> fail "error preparing query %S: %s" q e

  let query_str db q =
    let* stmt = stmt db q in
    match Sqlite3.step stmt with
    | ROW -> fail "unexpected Sqlite rows with query %S" q
    | c when Sqlite3.Rc.is_success c -> Lwt_result.return ()
    | rc -> fail_db db rc

  module Array = Array.Monadic2 (Lwt_result)

  let string_of_value : type t. t Sqml.Type.t -> t -> string = function
    | Sqml.Type.Int -> Int.to_string
    | Sqml.Type.Text -> Format.sprintf "%S"
    | Sqml.Type.Timestamp -> fun t -> Format.sprintf "%S" @@ Ptime.to_rfc3339 t
    | Sqml.Type.Varchar _ -> Format.sprintf "%S"

  let string_of_type : type t. t Sqml.Type.t -> string = function
    | Sqml.Type.Int -> "INTEGER"
    | Sqml.Type.Text -> "TEXT"
    | Sqml.Type.Timestamp -> "TEXT"
    | Sqml.Type.Varchar len -> Format.sprintf "VARCHAR(%i)" len

  let make ?(uri = ":memory:") () =
    try
      let db = Sqlite3.db_open uri in
      let create_table (module M : Table.Meta) =
        let rec column : type t. t Sqml.Column.List.t -> string list = function
          | Sqml.Column.List.Cons ({ Sqml.Column.name; type_ }, tl) ->
            let col = Format.sprintf "%s %s" name (string_of_type type_) in
            col :: column tl
          | Sqml.Column.List.Empty -> []
        in
        let module M = M in
        query_str db
          (Format.sprintf "CREATE TABLE IF NOT EXISTS %S (%s)" M.name
             (String.concat ~sep:", " (column M.columns)))
      in
      let* () = Array.iter ~f:create_table (Model.tables ()) in
      Lwt_result.return { db }
    with Sqlite3.Error m -> fail "%s" m

  let conv (type t) (type_ : t Sqml.Type.t) stmt i :
      (t, [> `Sqml_engine of string ]) Lwt_result.t =
    let get = function
      | Some v -> Lwt_result.return v
      | None -> Lwt_result.fail (`Sqml_engine "unexpected column type")
    in
    let column = Sqlite3.column stmt i in
    match type_ with
    | Sqml.Type.Int -> Sqlite3.Data.to_int column |> get
    | Sqml.Type.Text -> Sqlite3.Data.to_string column |> get
    | Sqml.Type.Timestamp ->
      let* repr = Sqlite3.Data.to_string column |> get in
      let* res, _, _ =
        let f (`RFC3339 (_, e)) =
          `Sqml_engine (Format.asprintf "%a" Ptime.pp_rfc3339_error e)
        in
        Ptime.of_rfc3339 repr |> Result.map_error ~f |> Lwt.return
      in
      Lwt_result.return res
    | Sqml.Type.Varchar _ -> Sqlite3.Data.to_string column |> get

  (* | Sql.Nullable t -> ( * try Some (conv t) with Invalid_argument _ -> None ) *)

  let insert
      (type mapped)
      { db }
      (module Table : Table.S with type Meta.mapped = mapped)
      (mapped : mapped) =
    let module T = Table in
    let cols = Table.Meta.unmap mapped in
    let rec columns : type t. t Sqml.Column.List.t -> t -> string list =
     fun specs cols ->
      match (specs, cols) with
      | Sqml.Column.List.Cons ({ type_; _ }, col_tl), (v, v_tl) ->
        let col = string_of_value type_ v in
        col :: columns col_tl v_tl
      | Sqml.Column.List.Empty, () -> []
    in
    query_str db
      (Format.sprintf "INSERT INTO %S VALUES (%s)" T.name
         (String.concat ~sep:", " (columns T.Meta.columns cols)))

  let query { db } q =
    let query_str = Fmt.str "%a" Model.Query.pp q in
    let* () = debug @@ fun m -> m "sqlite3: execute query: %s" query_str in
    let* stmt = stmt db query_str in
    match q with
    | Select (e, s) ->
      let module Selected = (val s) in
      let step stmt =
        match Sqlite3.step stmt with
        | ROW ->
          let rec get :
              type a.
              int ->
              (a, Selected.tag) Priv.Expression.t ->
              (int * a, [> `Sqml_engine of string ]) Lwt_result.t =
           fun i -> function
            | Priv.Expression.Column { column; _ } ->
              let+ res = conv column.type_ stmt i in
              (i + 1, res)
            | Priv.Expression.Pair (a, b) ->
              let* i, a = get i a in
              let+ i, b = get i b in
              (i, (a, b))
          in
          let* _, v = get 0 e in
          Lwt_result.return (Sequence.Step.Yield (v, stmt))
        | DONE -> Lwt_result.return Sequence.Step.Done
        | c when Sqlite3.Rc.is_success c ->
          fail "received Sqlite %s while fetching rows"
          @@ Sqlite3.Rc.to_string c
        | rc -> fail_db db rc
      in
      Lwt_result.return @@ Sequence.unfold ~init:stmt ~f:step
end
