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
  module Column : sig
    module Type : sig
      type 'a t =
        | Bool : bool t
        | Custom : {
            repr : 'repr t;
            encode : 'a -> ('repr, string) Result.t;
            decode : 'repr -> ('a, string) Result.t;
          }
            -> 'a t
        | Date : Timmy.Date.t t
        | Int : int t
        | Nullable : 't t -> 't option t
        | Text : string t
        | Time : Timmy.Time.t t
        | Varchar : int -> string t
    end

    type ('t, 'tag) t = private {
      selectable : string;
      name : string;
      type_ : 't Type.t;
    }

    module List : Hlist.S2 with type ('t, 'tag) value = ('t, 'tag) t
  end

  type free

  type 'a type_ =
    | Bool : bool type_
    | Date : Timmy.Date.t type_
    | Int : int type_
    | Nullable : 't type_ -> 't option type_
    | String : string type_
    | Time : Timmy.Time.t type_
    | Custom : {
        repr : 'repr type_;
        encode : 'a -> ('repr, string) Result.t;
        decode : 'repr -> ('a, string) Result.t;
      }
        -> 'a type_
    | Empty : unit Hlist.t type_
    | Pair : 'a type_ * 'b Hlist.t type_ -> ('a -> 'b) Hlist.t type_

  type ('t, 'tag) t = private
    | Column : ('t, 'tag) Column.t -> ('t, 'tag) t
    | Comparison : (comparison * ('t, 'tag) t * ('t, 'tag) t) -> (bool, 'tag) t
    | Concat : (string, 'tag) t * (string, 'tag) t -> (string, 'tag) t
    | Const : {
        type_ : 't type_;
        value : 't;
      }
        -> ('t, 'tag) t
    | Logic : (logic * (bool, 'tag) t * (bool, 'tag) t) -> (bool, 'tag) t
    | Map : {
        decode : 'mapped -> 't;
        encode : 't -> 'mapped;
        mapped : ('mapped, 'tag) t;
      }
        -> ('t, 'tag) t
    | Null : (_ option, 'tag) t
    | Var : 't type_ -> ('t, 'tag) t
    | ( :: ) :
        ('a, 'tag) t * ('b Hlist.t, 'tag) t
        -> (('a -> 'b) Hlist.t, 'tag) t
    | [] : (unit Hlist.t, 'tag) t

  type ('t, 'tag) option =
    | None : (unit, 'tag) option
    | Some : ('t, 'tag) t -> ('t, 'tag) option

  type 'tag assignment = private
    | Set : {
        column : ('t, 'tag) Column.t;
        value : ('t, 'tag) t;
      }
        -> 'tag assignment

  module List : Hlist.S2 with type ('t, 'tag) value = ('t, 'tag) t

  val column : ('t, 'tag) Column.t -> ('t, 'tag) t

  val const : 't type_ -> 't -> ('t, 'tag) t

  val int : int type_

  val map :
    decode:('mapped -> 't) ->
    encode:('t -> 'mapped) ->
    ('mapped, 'tag) t ->
    ('t, 'tag) t

  val null : (_ Option.t, 'tag) t

  val string : string type_

  val some : 't type_ -> 't Option.t type_

  val var : 't type_ -> ('t, 'tag) t (* FIXME *)

  val empty_list : (unit Hlist.t, 'tag) t

  module Infix : sig
    val ( && ) : (bool, 'tag) t -> (bool, 'tag) t -> (bool, 'tag) t

    val ( := ) : ('t, 'tag) Column.t -> ('t, 'tag) t -> 'tag assignment

    val ( < ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t

    val ( <= ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t

    val ( = ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t

    val ( > ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t

    val ( >= ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t

    val ( @ ) :
      ('a, 'tag) t -> ('b Hlist.t, 'tag) t -> (('a -> 'b) Hlist.t, 'tag) t

    val ( ^ ) : (string, 'tag) t -> (string, 'tag) t -> (string, 'tag) t

    val ( || ) : (bool, 'tag) t -> (bool, 'tag) t -> (bool, 'tag) t
  end

  include module type of Infix
end
