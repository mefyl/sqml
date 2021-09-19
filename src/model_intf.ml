open Acid
open Utils

module type S = sig
  val version : Semver.t

  type tag

  val tables : unit -> (module Table.S) array

  module BindTable (Meta : Table.META) :
    Table.S with type model = tag and module Meta = Meta

  module Join
      (L : Selectable.S with type model = tag)
      (R : Selectable.S with type model = tag) : sig
    include Selectable.S with type model = tag

    val left : ('t, L.tag) Expression.t -> ('t, tag) Expression.t

    val right : ('t, R.tag) Expression.t -> ('t, tag) Expression.t
  end

  module Query : sig
    type many = Many

    type zero = Zero

    type _ arity =
      | Many : many arity
      | Zero : zero arity

    type ('t, 'arity, 'error) result =
      | Many : ('t, 'error) Sequence.t -> ('t, many, 'error) result
      | Zero : ('t, zero, 'error) result

    type 'tag excluded = {
      excluded : 't. ('t, 'tag) Expression.t -> ('t, 'tag) Expression.t;
    }

    type 'tag on_conflict =
      | Ignore : _ on_conflict
      | Set :
          ('tag excluded -> 'tag Expression.assignment list)
          -> 'tag on_conflict
      | Update : _ on_conflict

    type ('t, 'arity) t = private
      | Delete : {
          from : (module Table.S with type model = tag);
          where : (bool, 'tag) Expression.t option;
        }
          -> (unit, zero) t
      | Delete_returning : {
          from : (module Table.S with type model = tag);
          where : (bool, 'tag) Expression.t option;
          returning : ('t, 'tag) Expression.t;
        }
          -> ('t, many) t
      | Insert : {
          excluded : 't. ('t, 'tag) Expression.t -> ('t, 'tag) Expression.t;
          into :
            (module Table.S
               with type model = tag
                and type Meta.mapped = 'mapped
                and type tag = 'tag);
          on_conflict : 'tag on_conflict option;
          returning : ('t, 'tag) Expression.option;
          value : ('mapped, Expression.free) Expression.t;
        }
          -> ('t, zero) t
      | Select : {
          from :
            (module Selectable.S with type model = tag and type tag = 'tag);
          what : ('t, 'tag) Expression.t;
          where : (bool, 'tag) Expression.t option;
        }
          -> ('t, many) t

    val arity : (_, 'arity) t -> 'arity arity
  end

  val select :
    from:(module Selectable.S with type model = tag and type tag = 'tag) ->
    ?where:(bool, 'tag) Expression.t ->
    ('t, 'tag) Expression.t ->
    ('t, Query.many) Query.t

  val insert :
    into:
      (module Table.S
         with type model = tag
          and type Meta.mapped = 'mapped
          and type tag = 'tag) ->
    ?on_conflict:'tag Query.on_conflict ->
    ('mapped, Expression.free) Expression.t ->
    (unit, Query.zero) Query.t

  val insert_returning :
    into:
      (module Table.S
         with type model = tag
          and type Meta.mapped = 'mapped
          and type tag = 'tag) ->
    returning:('t, 'tag) Expression.t ->
    ?on_conflict:'tag Query.on_conflict ->
    ('mapped, Expression.free) Expression.t ->
    ('t, Query.zero) Query.t

  val delete :
    from:(module Table.S with type model = tag) ->
    ?where:(bool, 'tag) Expression.t ->
    unit ->
    (unit, Query.zero) Query.t

  val delete_returning :
    from:(module Table.S with type model = tag) ->
    ?where:(bool, 'tag) Expression.t ->
    ('res, 'tag) Expression.t ->
    ('res, Query.many) Query.t
end

type t = (module S)
