include Dialect_intf

let nullable : type t. t Expression.Column.Type.atomic -> bool = function
  | Nullable _ -> true
  | _ -> false

module Make (D : sig
  val table_name : ?schema:string -> string -> string
  val index_name : ?schema:string -> string -> string
  val string_of_type : _ Expression.Column.Type.atomic -> string
end) =
struct
  include D

  let string_of_type ?(include_nullable = true) t =
    let type_ = string_of_type t in
    if (not include_nullable) || nullable t then type_
    else Fmt.str "%s NOT NULL" type_

  let pp_type fmt = Fmt.of_to_string (string_of_type ~include_nullable:true) fmt
end

module Postgresql : S = Make (struct
  let table_name ?schema name =
    match schema with
    | None -> Fmt.str "%S" name
    | Some schema -> Fmt.str "%S.%S" schema name

  let index_name ?schema:_ name = name

  let string_of_type t =
    let rec f : type t. t Expression.Column.Type.atomic -> string = function
      | Array element -> f element ^ "[]"
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
end)

let postgresql = (module Postgresql : S)

module Sqlite : S = Make (struct
  include Postgresql

  let table_name ?schema name =
    match schema with
    | None -> Fmt.str "%S" name
    | Some schema -> Fmt.str "%S" @@ Fmt.str "%s_%s" schema name

  let index_name = table_name

  let string_of_type t =
    let rec f : type t. t Expression.Column.Type.atomic -> string = function
      | Custom { repr; _ } -> f repr
      | Date | Time -> "TEXT"
      | t -> string_of_type t
    in
    f t
end)

let sqlite = (module Sqlite : S)
