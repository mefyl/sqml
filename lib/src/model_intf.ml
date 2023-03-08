open Utils

type many = Many
type one = One
type one_maybe = One_or_zero
type zero = Zero

type _ arity =
  | Many : many arity
  | One : one arity
  | One_maybe : one_maybe arity
  | Zero : zero arity

type ('t, 'arity, 'error) result =
  | Many : ('t, 'error) Sequence.t -> ('t, many, 'error) result
  | One : 't -> ('t, one, 'error) result
  | One_maybe : 't option -> ('t, one_maybe, 'error) result
  | Zero : ('t, zero, 'error) result

type 'tag excluded = {
  excluded : 't. ('t, 'tag) Expression.t -> ('t, 'tag) Expression.t;
}

type ('from, 'to_) mapper = {
  map : 't. ('t, 'from) Expression.t -> ('t, 'to_) Expression.t;
}

type 'tag on_conflict =
  | Ignore : _ on_conflict
  | Set : ('tag excluded -> 'tag Expression.assignment list) -> 'tag on_conflict
  | Update : _ on_conflict

module type S = sig
  val version : Semver.t

  type tag

  val tables : unit -> (module Table.S with type tag = tag) array

  module BindTable (Meta : Table.META) :
    Table.S with type model = tag and module Meta = Meta

  module type JOIN = sig
    include Selectable.S
    module L : Selectable.S with type model = model
    module R : Selectable.S with type model = model
  end

  module type INNER_JOIN = sig
    include JOIN

    val left : ('t, L.tag) Expression.t -> ('t, tag) Expression.t
    val right : ('t, R.tag) Expression.t -> ('t, tag) Expression.t
  end

  module type LEFT_OUTER_JOIN = sig
    include JOIN

    val left : ('t, L.tag) Expression.t -> ('t, tag) Expression.t
    val right : ('t, R.tag) Expression.t -> ('t Option.t, tag) Expression.t
  end

  module type RIGHT_OUTER_JOIN = sig
    include JOIN

    val left : ('t, L.tag) Expression.t -> ('t Option.t, tag) Expression.t
    val right : ('t, R.tag) Expression.t -> ('t, tag) Expression.t
  end

  module type OUTER_JOIN = sig
    include JOIN

    val left : ('t, L.tag) Expression.t -> ('t Option.t, tag) Expression.t
    val right : ('t, R.tag) Expression.t -> ('t Option.t, tag) Expression.t
  end

  module Join
      (L : Selectable.S with type model = tag)
      (R : Selectable.S with type model = tag) : sig
    module type INNER =
      INNER_JOIN with type model = tag and module L = L and module R = R

    module type OUTER =
      OUTER_JOIN with type model = tag and module L = L and module R = R

    module type LEFT_OUTER =
      LEFT_OUTER_JOIN with type model = tag and module L = L and module R = R

    module type RIGHT_OUTER =
      RIGHT_OUTER_JOIN with type model = tag and module L = L and module R = R

    include INNER

    val on : (bool, tag) Expression.t -> (module INNER)
    val outer_on : (bool, tag) Expression.t -> (module OUTER)
    val left_on : (bool, tag) Expression.t -> (module LEFT_OUTER)
    val right_on : (bool, tag) Expression.t -> (module RIGHT_OUTER)
  end

  module Query : sig
    type ('t, 'tag) delete = {
      from : (module Table.S with type model = tag and type tag = 'tag);
      where : (bool, 'tag) Expression.t option;
      returning : ('t, 'tag) Expression.t;
    }

    type ('t, 'tag) select = {
      for_ : Expression.for_ option;
      from : (module Selectable.S with type model = tag and type tag = 'tag);
      limit : int option;
      skip_locked : bool;
      what : ('t, 'tag) Expression.t;
      where : (bool, 'tag) Expression.t option;
    }

    type ('t, 'tag, 'from) update = {
      into : (module Table.S with type model = tag and type tag = 'tag);
      from :
        (module Selectable.S with type model = tag and type tag = 'from) option;
      what : 'tag Expression.assignment list;
      where : (bool, 'tag) Expression.t option;
      returning : ('t, 'tag) Expression.t;
    }

    type ('t, 'arity) t = private
      | Delete : {
          from : (module Table.S with type model = tag and type tag = 'tag);
          where : (bool, 'tag) Expression.t option;
        }
          -> (unit, zero) t
      | Delete_returning : ('t, 'tag) delete -> ('t, many) t
      | Delete_unique : ('t, 'tag) delete -> ('t, one_maybe) t
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
      | Select : ('t, 'tag) select -> ('t, many) t
      | Select_unique : ('t, 'tag) select -> ('t, one_maybe) t
      | Update : {
          into : (module Table.S with type model = tag and type tag = 'tag);
          what : 'tag Expression.assignment list;
          where : (bool, 'tag) Expression.t option;
        }
          -> (unit, zero) t
      | Update_returning : ('t, 'tag, 'from) update -> ('t, many) t
      | Update_unique : ('t, 'tag, 'from) update -> ('t, one_maybe) t

    val arity : (_, 'arity) t -> 'arity arity
  end

  val delete :
    from:(module Table.S with type model = tag and type tag = 'tag) ->
    ?where:(bool, 'tag) Expression.t ->
    unit ->
    (unit, zero) Query.t

  val delete_unique :
    from:(module Table.S with type model = tag and type tag = 'tag) ->
    ('indexed Table.Index.unique, 'tag) Table.Index.t ->
    ('indexed, 'tag) Expression.List_nullable.t ->
    ?where:(bool, 'tag) Expression.t ->
    ('res, 'tag) Expression.t ->
    ('res, one_maybe) Query.t

  val delete_returning :
    from:(module Table.S with type model = tag and type tag = 'tag) ->
    ?where:(bool, 'tag) Expression.t ->
    ('res, 'tag) Expression.t ->
    ('res, many) Query.t

  val insert :
    into:
      (module Table.S
         with type model = tag
          and type Meta.mapped = 'mapped
          and type tag = 'tag) ->
    ?on_conflict:'tag on_conflict ->
    ('mapped, 'free) Expression.t ->
    (unit, zero) Query.t

  val insert_returning :
    into:
      (module Table.S
         with type model = tag
          and type Meta.mapped = 'mapped
          and type tag = 'tag) ->
    returning:('t, 'tag) Expression.t ->
    ?on_conflict:'tag on_conflict ->
    ('mapped, 'free) Expression.t ->
    ('t, one) Query.t

  val select :
    from:(module Selectable.S with type model = tag and type tag = 'tag) ->
    ?for_:Expression.for_ ->
    ?limit:int ->
    ?skip_locked:bool ->
    ?where:(bool, 'tag) Expression.t ->
    ('t, 'tag) Expression.t ->
    ('t, many) Query.t

  val select_unique :
    from:(module Selectable.S with type model = tag and type tag = 'tag) ->
    ('indexed Table.Index.unique, 'tag) Table.Index.t ->
    ('indexed, 'tag) Expression.List_nullable.t ->
    ?for_:Expression.for_ ->
    ?skip_locked:bool ->
    ?where:(bool, 'tag) Expression.t ->
    ('t, 'tag) Expression.t ->
    ('t, one_maybe) Query.t

  val select_one :
    from:(module Selectable.S with type model = tag and type tag = 'tag) ->
    ?for_:Expression.for_ ->
    ?skip_locked:bool ->
    ?where:(bool, 'tag) Expression.t ->
    ('t, 'tag) Expression.t ->
    ('t, one_maybe) Query.t

  val update :
    (module Table.S with type model = tag and type tag = 'tag) ->
    ?where:(bool, 'tag) Expression.t ->
    'tag Expression.assignment list ->
    (unit, zero) Query.t

  val update_returning :
    (module Table.S with type model = tag and type tag = 'tag) ->
    ?where:(bool, 'tag) Expression.t ->
    returning:('t, 'tag) Expression.t ->
    'tag Expression.assignment list ->
    ('t, many) Query.t

  val update_unique :
    (module Table.S with type model = tag and type tag = 'tag) ->
    ('indexed Table.Index.unique, 'tag) Table.Index.t ->
    ('indexed, 'tag) Expression.List_nullable.t ->
    ?where:(bool, 'tag) Expression.t ->
    returning:('t, 'tag) Expression.t ->
    'tag Expression.assignment list ->
    ('t, one_maybe) Query.t

  val update_from_returning :
    (module Table.S with type model = tag and type tag = 'tag) ->
    from:(module Selectable.S with type model = tag and type tag = 'from) ->
    ?where:(('from, 'tag) mapper -> (bool, 'tag) Expression.t) ->
    returning:(('from, 'tag) mapper -> ('t, 'tag) Expression.t) ->
    (('from, 'tag) mapper -> 'tag Expression.assignment list) ->
    ('t, many) Query.t

  val update_from_unique :
    (module Table.S with type model = tag and type tag = 'tag) ->
    ('indexed Table.Index.unique, 'tag) Table.Index.t ->
    ('indexed, 'tag) Expression.List_nullable.t ->
    from:(module Selectable.S with type model = tag and type tag = 'from) ->
    ?where:(('from, 'tag) mapper -> (bool, 'tag) Expression.t) ->
    returning:(('from, 'tag) mapper -> ('t, 'tag) Expression.t) ->
    (('from, 'tag) mapper -> 'tag Expression.assignment list) ->
    ('t, one_maybe) Query.t
end

type t = (module S)
