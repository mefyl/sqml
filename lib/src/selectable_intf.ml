(** Any object that can be selected from *)

module type S = Expression.SELECTABLE

type 'tag t = (module S with type tag = 'tag)
