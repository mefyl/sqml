val select : 'tag Selectable.t -> ('t, 'tag) Expression.t -> 't Query.t

module Join (L : Selectable.S) (R : Selectable.S) : sig
  include Selectable.S

  val left : ('t, L.tag) Expression.t -> ('t, tag) Expression.t

  val right : ('t, R.tag) Expression.t -> ('t, tag) Expression.t
end
