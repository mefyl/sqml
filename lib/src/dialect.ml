include Dialect_intf

module Postgresql : S = struct
  let table_name ?schema name =
    match schema with
    | None -> Fmt.str "%S" name
    | Some schema -> Fmt.str "%S.%S" schema name

  let index_name ?schema:_ name = name

  let string_of_type t =
    let rec f : type t. t Expression.Column.Type.t -> string = function
      | Bigint -> "BIGINT"
      | Bool -> "BOOLEAN"
      | Custom { repr; _ } -> f repr
      | Date -> "DATE"
      | Float -> "REAL"
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
  include Postgresql

  let table_name ?schema name =
    match schema with
    | None -> Fmt.str "%S" name
    | Some schema -> Fmt.str "%S" @@ Fmt.str "%s_%s" schema name

  let index_name = table_name

  let string_of_type t =
    let rec f : type t. t Expression.Column.Type.t -> string = function
      | Custom { repr; _ } -> f repr
      | Date
      | Time ->
        "TEXT"
      | t -> string_of_type t
    in
    f t
end

let sqlite = (module Sqlite : S)
