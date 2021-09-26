open Acid

module type Interface = sig
  module Column : sig
    module Type : sig
      type 'a t =
        | Bool : bool t
        | Custom : {
            repr : 'repr t;
            encode : 'a -> 'repr;
            decode : 'repr -> ('a, string) Result.t;
          }
            -> 'a t
        | Date : Timmy.Date.t t
        | Int : int t
        | Json : Schematic.Json.t t
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

  module Type : sig
    type 'a t =
      | Bool : bool t
      | Date : Timmy.Date.t t
      | Int : int t
      | Json : Schematic.Json.t t
      | Null : _ option t
      | Nullable : 't t -> 't option t
      | String : string t
      | Time : Timmy.Time.t t
      | Custom : {
          repr : 'repr t;
          encode : 'a -> 'repr;
          decode : 'repr -> ('a, string) Result.t;
        }
          -> 'a t
      | [] : unit Hlist.t t
      | ( :: ) : 'a t * 'b Hlist.t t -> ('a, 'b) Hlist.desc Hlist.t t

    module List : Hlist.S with type 't value = 't t

    val of_column_type : 't Column.Type.t -> 't t

    val pp : 't t Fmt.t
  end

  type ast =
    | Column : {
        selectable : string;
        name : string;
      }
        -> ast
    | Const : {
        type_ : 't Type.t;
        value : 't;
      }
        -> ast
    | Funcall : {
        name : string;
        args : ast list;
      }
        -> ast
    | Infix : {
        op : string;
        lhs : ast;
        rhs : ast;
      }
        -> ast
    | List : ast list -> ast
    | Null : ast
    | Switch : {
        cases : (ast * ast) list;
        default : ast Option.t;
      }
        -> ast
    | Var : {
        index : int;
        size : int;
      }
        -> ast

  type ('t, 'tag) t

  val case :
    ((bool, 'tag) t * ('t Option.t, 'tag) t) list -> ('t Option.t, 'tag) t

  val case_else :
    ((bool, 'tag) t * ('t, 'tag) t) list -> ('t, 'tag) t -> ('t, 'tag) t

  val to_ast : ('t, 'tag) t -> ast

  val to_type : ('t, 'tag) t -> 't Type.t

  type ('t, 'tag) option =
    | None : (unit, 'tag) option
    | Some : ('t, 'tag) t -> ('t, 'tag) option

  type 'tag assignment

  val assignment_to_ast : 'tag assignment -> string * ast

  module List : Hlist.S2 with type ('t, 'tag) value = ('t, 'tag) t

  val of_list : ('l, 'tag) List.t -> ('l Hlist.t, 'tag) t

  val coalesce : ('t Option.t, 'tag) t list -> ('t Option.t, 'tag) t

  val coalesce_default :
    ('t Option.t, 'tag) t list -> ('t, 'tag) t -> ('t, 'tag) t

  val column : ('t, 'tag) Column.t -> ('t, 'tag) t

  val const : 't Type.t -> 't -> ('t, 'tag) t

  val map :
    decode:('mapped -> ('t, string) Result.t) ->
    encode:('t -> 'mapped) ->
    ('mapped, 'tag) t ->
    ('t, 'tag) t

  val null : (_ Option.t, 'tag) t

  (* FIXME: private *)
  val prepare : 'args Type.List.t -> ('args, 'free) List.t

  module Infix : sig
    val ( && ) : (bool, 'tag) t -> (bool, 'tag) t -> (bool, 'tag) t

    val ( := ) : ('t, 'tag) Column.t -> ('t, 'tag) t -> 'tag assignment

    val ( < ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t

    val ( <= ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t

    val ( = ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t

    val ( > ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t

    val ( >= ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t

    val ( ^ ) : (string, 'tag) t -> (string, 'tag) t -> (string, 'tag) t

    val ( || ) : (bool, 'tag) t -> (bool, 'tag) t -> (bool, 'tag) t
  end

  include module type of Infix
end
