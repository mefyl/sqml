open Acid

type comparison =
  | Eq
  | Gt
  | Gte
  | Lt
  | Lte

type logic =
  | And
  | Or

module type Interface = sig
  type ('t, 'tag) column = private {
    selectable : string;
    column : 't Column.t;
  }

  type ('t, 'tag) t = private
    | Column : ('t, 'tag) column -> ('t, 'tag) t
    | Comparison : (comparison * ('t, 'tag) t * ('t, 'tag) t) -> (bool, 'tag) t
    | Int : int -> (int, 'tag) t
    | Logic : (logic * (bool, 'tag) t * (bool, 'tag) t) -> (bool, 'tag) t
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

  val column : ('t, 'tag) column -> ('t, 'tag) t

  val int : int -> (int, 'tag) t

  val map :
    decode:('mapped -> 't) ->
    encode:('t -> 'mapped) ->
    ('mapped, 'tag) t ->
    ('t, 'tag) t

  val string : string -> (string, 'tag) t

  val null : (unit, 'tag) t

  val var : unit -> ('t, 'tag) t (* FIXME *)

  val ( @ ) : ('a, 'tag) t -> ('b, 'tag) t -> ('a * 'b, 'tag) t

  val ( = ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t

  val ( && ) : (bool, 'tag) t -> (bool, 'tag) t -> (bool, 'tag) t

  val ( || ) : (bool, 'tag) t -> (bool, 'tag) t -> (bool, 'tag) t
end
