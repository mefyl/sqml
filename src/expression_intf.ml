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
    name : string;
    type_ : 't Type.t;
  }

  module Column_list : S_list.S2 with type ('t, 'tag) value = ('t, 'tag) column

  type ('t, 'tag) t = private
    | Column : ('t, 'tag) column -> ('t, 'tag) t
    | Comparison : (comparison * ('t, 'tag) t * ('t, 'tag) t) -> (bool, 'tag) t
    | Concat : (string, 'tag) t * (string, 'tag) t -> (string, 'tag) t
    | Int : int -> (int, 'tag) t
    | Logic : (logic * (bool, 'tag) t * (bool, 'tag) t) -> (bool, 'tag) t
    | Map : {
        decode : 'mapped -> 't;
        encode : 't -> 'mapped;
        mapped : ('mapped, 'tag) t;
      }
        -> ('t, 'tag) t
    | String : string -> (string, 'tag) t
    | Unit : (unit, 'tag) t
    | Var : (_, 'tag) t
    | ( :: ) :
        ('a, 'tag) t * ('b H_list.t, 'tag) t
        -> (('a -> 'b) H_list.t, 'tag) t
    | [] : (unit H_list.t, 'tag) t

  type 'tag assignment = private
    | Set : {
        column : ('t, 'tag) column;
        value : ('t, 'tag) t;
      }
        -> 'tag assignment

  module List : S_list.S2 with type ('t, 'tag) value = ('t, 'tag) t

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

  val empty_list : (unit H_list.t, 'tag) t

  module Infix : sig
    val ( && ) : (bool, 'tag) t -> (bool, 'tag) t -> (bool, 'tag) t

    val ( := ) : ('t, 'tag) column -> ('t, 'tag) t -> 'tag assignment

    val ( = ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t

    val ( @ ) :
      ('a, 'tag) t -> ('b H_list.t, 'tag) t -> (('a -> 'b) H_list.t, 'tag) t

    val ( ^ ) : (string, 'tag) t -> (string, 'tag) t -> (string, 'tag) t

    val ( || ) : (bool, 'tag) t -> (bool, 'tag) t -> (bool, 'tag) t
  end

  include module type of Infix
end
