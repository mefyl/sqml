include module type of Table_intf

val pp_referential_action : referential_action Fmt.t
val referential_action_of_string : string -> referential_action option
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

  module Constraint : sig
    include module type of Constraint
    include Base.Comparable.S with type t := t
  end

  val equal_column : column -> column -> bool
  val of_table : (module S with type tag = 'tag) -> t
end

module Query : sig
  type t

  val create : (module S with type tag = 'tag) -> t list
  val drop : (module S with type tag = 'tag) -> t list

  val drop_column :
    (module S with type tag = 'tag) -> (_, 'tag) Expression.Column.t -> t list

  val pp : Dialect.t -> t Fmt.t

  module Schema : sig
    val add_constraint : Schema.name -> Schema.Constraint.t -> t
    val alter_column_nullable : Schema.name -> string -> bool -> t

    val alter_column_type :
      Schema.name ->
      string ->
      Expression.Column.Type.any ->
      Expression.Column.Type.any ->
      t

    val create : Schema.t -> t list
    val drop : Schema.t -> t list
    val drop_column : Schema.name -> Schema.column -> t list
    val drop_constraint : Schema.name -> string -> t list
  end
end
