module type S = sig
  val table_name : ?schema:string -> string -> string
  val index_name : ?schema:string -> string -> string

  val string_of_type :
    ?include_nullable:bool -> _ Expression.Column.Type.atomic -> string

  val pp_from_type : 't Expression.Type.t -> 't Fmt.t
  val pp_type : _ Expression.Column.Type.atomic Fmt.t
  val pp : Expression.ast Fmt.t
end

type t = (module S)
