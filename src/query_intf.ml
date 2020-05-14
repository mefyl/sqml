open Base

module type Interface = sig
  type 't t

  val pp : Formatter.t -> 't t -> unit
end

type 't t = Select : ('t, 'tag) Expression.t * 'tag Selectable.t -> 't t
