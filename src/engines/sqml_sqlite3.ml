open Acid
open Sqml
module Sequence = Engine.Sequence

module Make (Model : Model.S) = struct
  type 'model t = { db : Sqlite3.db }

  module Model = Model

  let ( let* ) = Lwt_result.( >>= )

  let ( let+ ) = Lwt_result.( >>| )

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

  let string_of_type : type t. t Sqml.Type.t -> string = function
    | Sqml.Type.Int -> "INT"
    | Sqml.Type.Text -> "TEXT"

  let make () =
    try
      let db = Sqlite3.db_open ":memory:" in
      let create_table (module M : Table.Meta) =
        let rec column : type t. t Sqml.Column.List.t -> string list = function
          | Sqml.Column.List.Cons ({ Sqml.Column.name; type_ }, tl) ->
            let col = Format.sprintf "%s %s" name (string_of_type type_) in
            col :: column tl
          | Sqml.Column.List.Empty -> []
        in
        let module M = M in
        query_str db
          (Format.sprintf "CREATE TABLE %S (%s)" M.name
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
    | Sqml.Type.Text -> Sqlite3.Data.to_string column |> get
    | Sqml.Type.Int -> Sqlite3.Data.to_int column |> get

  (* | Sql.Nullable t -> ( * try Some (conv t) with Invalid_argument _ -> None ) *)

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
