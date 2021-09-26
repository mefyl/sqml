include Dialect_intf

module Postgresql : S = struct
  let table_name ?schema name =
    match schema with
    | None -> Fmt.str "%S" name
    | Some schema -> Fmt.str "%S.%S" schema name

  let string_of_type t =
    let rec f : type t. t Expression.Column.Type.t -> string = function
      | Bool -> "BOOLEAN"
      | Custom { repr; _ } -> f repr
      | Date -> "DATE"
      | Int -> "INTEGER"
      | Json -> "JSONB"
      | Nullable t -> f t
      | Text -> "TEXT"
      | Time -> "TIMESTAMP"
      | Varchar len -> Fmt.str "VARCHAR(%i)" len
    in
    f t
end

let postgresql = (module Postgresql : S)

module Sqlite : S = struct
  let table_name ?schema name =
    match schema with
    | None -> Fmt.str "%S" name
    | Some schema -> Fmt.str "%S_%S" schema name

  let string_of_type t =
    let rec f : type t. t Expression.Column.Type.t -> string = function
      | Custom { repr; _ } -> f repr
      | Date
      | Time ->
        "TEXT"
      | t -> Postgresql.string_of_type t
    in
    f t
end

let sqlite = (module Sqlite : S)
