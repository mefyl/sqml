module type S = sig
  val table_name : ?schema:string -> string -> string

  val index_name : ?schema:string -> string -> string

  val string_of_type : _ Expression.Column.Type.t -> string
end

type t = (module S)
