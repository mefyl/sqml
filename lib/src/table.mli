include module type of Table_intf

val pp_ordering : _ ordering Fmt.t
val pp_referential_action : referential_action Fmt.t
val referential_action_of_string : string -> referential_action option
val name : (module S' with type tag = _) -> string

module Column : sig
  include module type of Column

  val make :
    ?foreign_keys:'a Foreign_key.t list -> string -> 'a Type.atomic -> 'a t

  val make_nullable :
    ?foreign_keys:'a option Foreign_key.t list ->
    string ->
    'a Type.atomic ->
    'a option t

  val make_composite :
    ?foreign_keys:'a Foreign_key.t list -> string -> 'a Type.t -> 'a t

  module Foreign_key : sig
    include module type of Foreign_key

    val make :
      ?deferrability:deferrability ->
      ?on_delete:referential_action ->
      ?on_update:referential_action ->
      ('a, 'b) reference ->
      'a t

    val make_nullable :
      ?deferrability:deferrability ->
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

  type primary

  val primary :
    (columns, 'tag) Expression.Column.List.t ->
    (primary, 'tag) Expression.Column.List.t
end) :
  META with type columns = Minimal.columns and type primary = Minimal.primary

module Schema : sig
  include module type of Schema

  module Constraint : sig
    include module type of Constraint
    include Base.Comparable.S with type t := t
  end

  module Index : sig
    include module type of Index
    include Base.Comparable.S with type t := t
  end

  val unroll_column : column -> column_atomic list
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
    val add_column : Schema.name -> Schema.column_atomic -> t
    val add_constraint : Schema.name -> Schema.Constraint.t -> t
    val alter_column_nullable : Schema.name -> string -> bool -> t

    val alter_column_type :
      Schema.name ->
      string ->
      Expression.Column.Type.any_atomic ->
      Expression.Column.Type.any_atomic ->
      t

    val create : Schema.t -> t list
    val create_index : Schema.name -> Schema.Index.t -> t list
    val drop : Schema.t -> t list
    val drop_column : Schema.name -> Schema.column_atomic -> t list
    val drop_constraint : Schema.name -> string -> t list
    val drop_index : Schema.name -> t list
  end
end
