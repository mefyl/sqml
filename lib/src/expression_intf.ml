open Utils

module Column = struct
  module Type = struct
    type 'a atomic =
      | Array : 't atomic -> 't list atomic
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
      | Serial : int atomic
      | Serial_big : Int64.t atomic
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
      | Custom : {
          repr : 'repr t;
          encode : 'a -> 'repr;
          decode : 'repr -> ('a, string) Result.t;
        }
          -> 'a t
      | Nullable : 'a t -> 'a option t

    type any = Type : _ t -> any
  end
end

module Type = struct
  type 'a t =
    | Array : 't t -> 't list t
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

  let rec pp : 'a. 'a t Fmt.t =
   fun (type a) f (t : a t) ->
    let open Fmt in
    match t with
    | Array t -> pf f "%a ARRAY" pp t
    | Bigint -> string f "BIGINT"
    | Bool -> string f "BOOL"
    | Date -> string f "DATE"
    | Float -> string f "FLOAT"
    | Int -> string f "INT"
    | Json -> string f "JSON"
    | Null -> string f "NULL"
    | Nullable t -> pf f "%a NULLABLE" pp t
    | String -> string f "STRING"
    | Time -> string f "TIME"
    | Custom { repr; _ } -> pf f "%a CUSTOM" pp repr
    | [] -> string f "[]"
    | hd :: tl -> pf f "%a :: %a" pp hd pp tl

  module List = Hlist.Make (struct
    type nonrec 't t = 't t
  end)
end

type ast =
  | Column of {
      selectable : string;
      names : string list;
    }
  | Const : {
      type_ : 't Type.t; [@printer Fmt.nop]
      value : 't; [@printer Fmt.nop]
    }
      -> ast
  | Default
  | Funcall of {
      name : string;
      args : ast list;
    }
  | Infix of {
      op : string;
      lhs : ast;
      rhs : ast;
      precedence : int;
    }
  | Is_null of ast
  | Is_not_null of ast
  | Join of {
      left : ast;
      right : ast;
      on : ast option;
      type_ : join_type;
    }
  | List of ast list
  | Null
  | Select of {
      for_ : for_ option;
      from : ast;
      group_by : ast option;
      limit : int option;
      skip_locked : bool;
      what : ast;
      where : ast option;
    }
  | Selectable of {
      name : string;
      alias : string option;
    }
  | Star
  | Switch of {
      cases : (ast * ast) list;
      default : ast option;
    }
  | Var : {
      index : int;
      size : int;
      type_ : _ Type.t; [@printer Fmt.nop]
    }
      -> ast

and for_ =
  | Share
  | Update

and join_type =
  | Inner
  | Outer
  | Outer_left
  | Outer_right
[@@deriving show]

module type SELECTABLE = sig
  type model

  (** Type identifier of columns that can be selected from this *)
  type tag

  val to_ast : unit -> ast
end
