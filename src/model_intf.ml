open Acid
open Utils

module type S = sig
  val version : Semver.t

  type tag

  val tables : unit -> (module Table.S) array

  module BindTable (Meta : Table.META) :
    Table.S with type model = tag and module Meta = Meta

  module type JOIN = sig
    include Selectable.S

    module L : Selectable.S with type model = model

    module R : Selectable.S with type model = model

    val left : ('t, L.tag) Expression.t -> ('t, tag) Expression.t

    val right : ('t, R.tag) Expression.t -> ('t, tag) Expression.t
  end

  module Join
      (L : Selectable.S with type model = tag)
      (R : Selectable.S with type model = tag) : sig
    module type S = JOIN with type model = tag and module L = L and module R = R

    include S

    val on : (bool, tag) Expression.t -> (module S)
  end

  module Query : sig
    type many = Many

    type one = One

    type zero = Zero

    type _ arity =
      | Many : many arity
      | One : one arity
      | Zero : zero arity

    type ('t, 'arity, 'error) result =
      | Many : ('t, 'error) Sequence.t -> ('t, many, 'error) result
      | One : 't -> ('t, one, 'error) result
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
          value : ('mapped, 'free) Expression.t;
        }
          -> (unit, zero) t
      | Insert_returning : {
          excluded : 't. ('t, 'tag) Expression.t -> ('t, 'tag) Expression.t;
          into :
            (module Table.S
               with type model = tag
                and type Meta.mapped = 'mapped
                and type tag = 'tag);
          on_conflict : 'tag on_conflict option;
          returning : ('t, 'tag) Expression.t;
          value : ('mapped, 'free) Expression.t;
        }
          -> ('t, one) t
      | Select : {
          from :
            (module Selectable.S with type model = tag and type tag = 'tag);
          what : ('t, 'tag) Expression.t;
          where : (bool, 'tag) Expression.t option;
        }
          -> ('t, many) t
      | Update : {
          into : (module Table.S with type model = tag and type tag = 'tag);
          what : 'tag Expression.assignment list;
          where : (bool, 'tag) Expression.t option;
        }
          -> (unit, zero) t
      | Update_returning : {
          into : (module Table.S with type model = tag and type tag = 'tag);
          what : 'tag Expression.assignment list;
          where : (bool, 'tag) Expression.t option;
          returning : ('t, 'tag) Expression.t;
        }
          -> ('t, many) t

    val arity : (_, 'arity) t -> 'arity arity
  end

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

  val insert :
    into:
      (module Table.S
         with type model = tag
          and type Meta.mapped = 'mapped
          and type tag = 'tag) ->
    ?on_conflict:'tag Query.on_conflict ->
    ('mapped, 'free) Expression.t ->
    (unit, Query.zero) Query.t

  val insert_returning :
    into:
      (module Table.S
         with type model = tag
          and type Meta.mapped = 'mapped
          and type tag = 'tag) ->
    returning:('t, 'tag) Expression.t ->
    ?on_conflict:'tag Query.on_conflict ->
    ('mapped, 'free) Expression.t ->
    ('t, Query.one) Query.t

  val select :
    from:(module Selectable.S with type model = tag and type tag = 'tag) ->
    ?where:(bool, 'tag) Expression.t ->
    ('t, 'tag) Expression.t ->
    ('t, Query.many) Query.t

  val update :
    (module Table.S with type model = tag and type tag = 'tag) ->
    ?where:(bool, 'tag) Expression.t ->
    'tag Expression.assignment list ->
    (unit, Query.zero) Query.t

  val update_returning :
    (module Table.S with type model = tag and type tag = 'tag) ->
    ?where:(bool, 'tag) Expression.t ->
    returning:('t, 'tag) Expression.t ->
    'tag Expression.assignment list ->
    ('t, Query.many) Query.t
end

type t = (module S)
