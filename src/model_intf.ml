open Base

module type S = sig
  val version : Semver.t

  type tag

  val tables : unit -> (module Table.Meta) array

  module BindTable (Meta : Table.Meta) :
    Table.S with type model = tag and module Meta = Meta

  module Join
      (L : Selectable.S with type model = tag)
      (R : Selectable.S with type model = tag) : sig
    include Selectable.S with type model = tag

    val left : ('t, L.tag) Expression.t -> ('t, tag) Expression.t

    val right : ('t, R.tag) Expression.t -> ('t, tag) Expression.t
  end

  module Query : sig
    type 't t =
      (* FIXME: should be private *)
      | Select :
          ('t, 'tag) Expression.t
          * (module Selectable.S with type model = tag and type tag = 'tag)
          * (bool, 'tag) Expression.t option
          -> 't t

    val pp : Formatter.t -> 't t -> unit
  end

  val select :
    from:(module Selectable.S with type model = tag and type tag = 'tag) ->
    ?where:(bool, 'tag) Expression.t ->
    ('t, 'tag) Expression.t ->
    't Query.t
end
