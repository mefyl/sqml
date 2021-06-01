open Acid

type comparison =
  | Eq
  | Gt
  | Gte
  | Lt
  | Lte

module type Interface = sig
  type ('a, 'tag) t = private
    | Column : {
        selectable : string;
        column : 't Column.t;
      }
        -> ('t, 'tag) t
    | Comparison : (comparison * ('t, 'tag) t * ('t, 'tag) t) -> (bool, 'tag) t
    | Int : int -> (int, 'tag) t
    | Map : {
        decode : 'mapped -> 't;
        encode : 't -> 'mapped;
        mapped : ('mapped, 'tag) t;
      }
        -> ('t, 'tag) t
    | Pair : ('a, 'tag) t * ('b, 'tag) t -> ('a * 'b, 'tag) t
    | String : string -> (string, 'tag) t
    | Unit : (unit, 'tag) t
    | Var : (_, 'tag) t

  val pp : Formatter.t -> ('t, 'tag) t -> unit

  val int : int -> (int, 'tag) t

  val map :
    decode:('mapped -> 't) ->
    encode:('t -> 'mapped) ->
    ('mapped, 'tag) t ->
    ('t, 'tag) t

  val string : string -> (string, 'tag) t

  val unit : (unit, 'tag) t

  val var : unit -> ('t, 'tag) t (* FIXME *)

  val ( @ ) : ('a, 'tag) t -> ('b, 'tag) t -> ('a * 'b, 'tag) t

  val ( = ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t
end
