include Dialect_intf

module Postgresql : S = struct
  let table_name ?schema name =
    match schema with
    | None -> Fmt.str "%S" name
    | Some schema -> Fmt.str "%S.%S" schema name

  let string_of_type t =
    let rec f : type t. t Type.t -> string = function
      | Type.Bool -> "BOOLEAN"
      | Type.Custom { repr; _ } -> f repr
      | Type.Int -> "INTEGER"
      | Type.Nullable t -> f t
      | Type.Text -> "TEXT"
      | Type.Time -> "TIMESTAMP"
      | Type.Varchar len -> Fmt.str "VARCHAR(%i)" len
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
    let rec f : type t. t Type.t -> string = function
      | Type.Custom { repr; _ } -> f repr
      | Type.Time -> "TEXT"
      | t -> Postgresql.string_of_type t
    in
    f t
end

let sqlite = (module Sqlite : S)
