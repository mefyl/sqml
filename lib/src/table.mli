include module type of Table_intf

val pp_referential_action : referential_action Fmt.t
val name : (module S' with type tag = _) -> string

module Column : sig
  include module type of Column

  val make :
    ?primary:bool ->
    ?foreign_keys:'a Foreign_key.t list ->
    string ->
    'a Type.t ->
    'a t

  val make_nullable :
    ?primary:bool ->
    ?foreign_keys:'a option Foreign_key.t list ->
    string ->
    'a Type.t ->
    'a option t

  module Foreign_key : sig
    include module type of Foreign_key

    val make :
      ?on_delete:referential_action ->
      ?on_update:referential_action ->
      ('a, 'b) reference ->
      'a t

    val make_nullable :
      ?on_delete:referential_action ->
      ?on_update:referential_action ->
      ('a, 'b) reference ->
      'a option t
  end
end

module Meta (Minimal : sig
  val name : string

  type columns

  val columns : columns Column.List.t
end) : META with type columns = Minimal.columns

module Schema : sig
  include module type of Schema

  val equal_column : column -> column -> bool
  val of_table : (module S) -> t
end

module Query : sig
  type t

  val pp : Dialect.t -> t Fmt.t
  val create : (module S) -> t list
end
