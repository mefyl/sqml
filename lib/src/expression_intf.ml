open Utils

module Column = struct
  module Type = struct
    type 'a atomic =
      | Bigint : Int64.t atomic
      | Bool : bool atomic
      | Custom : {
          repr : 'repr atomic;
          encode : 'a -> 'repr;
          decode : 'repr -> ('a, string) Result.t;
        }
          -> 'a atomic
      | Date : Timmy.Date.t atomic
      | Float : float atomic
      | Int : int atomic
      | Json : Ezjsonm.value atomic
      | Nullable : 't atomic -> 't option atomic
      | Text : string atomic
      | Time : Timmy.Time.t atomic
      | Varchar : int -> string atomic

    type any_atomic = Type : _ atomic -> any_atomic

    module List = Hlist.Make (struct
      type nonrec 't t = string * 't atomic
    end)

    type 'a t =
      | Atomic : 'a atomic -> 'a t
      | Composite : {
          decode : 'l Hlist.t -> 'a;
          encode : 'a -> 'l Hlist.t;
          columns : 'l List.t;
        }
          -> 'a t

    type any = Type : _ t -> any
  end
end

module Type = struct
  type 'a t =
    | Bigint : Int64.t t
    | Bool : bool t
    | Date : Timmy.Date.t t
    | Float : float t
    | Int : int t (* FIXME: should be int32 ? *)
    | Json : Ezjsonm.value t
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
    | ( :: ) : 'a t * 'b Hlist.t t -> ('a -> 'b) Hlist.t t

  module List = Hlist.Make (struct
    type nonrec 't t = 't t
  end)
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
  | Is_null : ast -> ast
  | Is_not_null : ast -> ast
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
