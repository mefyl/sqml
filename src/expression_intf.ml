open Base

module type Interface = sig
  type ('t, 'tag) t

  val pp : Formatter.t -> ('t, 'tag) t -> unit

  val ( @ ) : ('a, 'tag) t -> ('b, 'tag) t -> ('a * 'b, 'tag) t
end

type ('a, 'tag) t =
  | Column : {
      selectable : string;
      column : 't Column.t;
    }
      -> ('t, 'tag) t
  | Pair : ('a, 'tag) t * ('b, 'tag) t -> ('a * 'b, 'tag) t
