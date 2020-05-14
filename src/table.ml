module type Meta = sig
  val name : string

  type columns

  val columns : columns Column.List.t
end

module type S = sig
  include Selectable.S

  val name : string

  module Expression : sig
    type 't t = ('t, tag) Expression.t

    module List : module type of S_list.Make (struct
      type 't t = ('t, tag) Expression.t
    end)
  end

  type columns

  val columns : columns Expression.List.t
end

type 'tag t = (module S with type tag = 'tag)
