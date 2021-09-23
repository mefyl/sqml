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

  module Type : sig
    type 'a t =
      | Bool : bool t
      | Date : Timmy.Date.t t
      | Int : int t
      | Nullable : 't t -> 't option t
      | String : string t
      | Time : Timmy.Time.t t
      | Custom : {
          repr : 'repr t;
          encode : 'a -> ('repr, string) Result.t;
          decode : 'repr -> ('a, string) Result.t;
        }
          -> 'a t
      | [] : unit Hlist.t t
      | ( :: ) : 'a t * 'b Hlist.t t -> ('a, 'b) Hlist.desc Hlist.t t

    module List : Hlist.S with type 't value = 't t
  end

  type ('t, 'tag) t = private
    | Column : ('t, 'tag) Column.t -> ('t, 'tag) t
    | Comparison : (comparison * ('t, 'tag) t * ('t, 'tag) t) -> (bool, 'tag) t
    | Concat : (string, 'tag) t * (string, 'tag) t -> (string, 'tag) t
    | Const : {
        type_ : 't Type.t;
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
    | Var : 't Type.t -> ('t, 'tag) t
    | ( :: ) :
        ('a, 'tag) t * ('b Hlist.t, 'tag) t
        -> (('a, 'b) Hlist.desc Hlist.t, 'tag) t
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

  val const : 't Type.t -> 't -> ('t, 'tag) t

  val map :
    decode:('mapped -> 't) ->
    encode:('t -> 'mapped) ->
    ('mapped, 'tag) t ->
    ('t, 'tag) t

  val null : (_ Option.t, 'tag) t

  val var : 't Type.t -> ('t, 'tag) t (* FIXME *)

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
      ('a, 'tag) t ->
      ('b Hlist.t, 'tag) t ->
      (('a, 'b) Hlist.desc Hlist.t, 'tag) t

    val ( ^ ) : (string, 'tag) t -> (string, 'tag) t -> (string, 'tag) t

    val ( || ) : (bool, 'tag) t -> (bool, 'tag) t -> (bool, 'tag) t
  end

  include module type of Infix
end
