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

    let rec equal_atomic : type l r. l atomic -> r atomic -> bool =
     fun l r ->
      match (l, r) with
      | Bigint, Bigint
      | Bool, Bool
      | Date, Date
      | Float, Float
      | Int, Int
      | Json, Json
      | Serial, Serial
      | Serial_big, Serial_big
      | Text, Text
      | Time, Time ->
        true
      | Array l, Array r -> equal_atomic l r
      | Custom { repr; _ }, r -> equal_atomic repr r
      | l, Custom { repr; _ } -> equal_atomic l repr
      | Nullable l, Nullable r -> equal_atomic l r
      | Varchar l, Varchar r -> l = r
      | Bigint, _
      | Bool, _
      | Date, _
      | Float, _
      | Int, _
      | Json, _
      | Serial, _
      | Serial_big, _
      | Text, _
      | Time, _
      | Array _, _
      | Nullable _, _
      | Varchar _, _ ->
        false

    let equal_any_atomic (Type l) (Type r) = equal_atomic l r

    let rec pp_atomic : type t. t atomic Fmt.t =
     fun f -> function
      | Array t -> Fmt.pf f "%a[]" pp_atomic t
      | Bigint -> Fmt.string f "BIGINT"
      | Bool -> Fmt.string f "BOOLEAN"
      | Custom { repr; _ } -> pp_atomic f repr
      | Date -> Fmt.string f "DATE"
      | Float -> Fmt.string f "REAL"
      | Int -> Fmt.string f "INTEGER"
      | Json -> Fmt.string f "JSONB"
      | Nullable t -> Fmt.pf f "%a?" pp_atomic t
      | Serial -> Fmt.string f "SERIAL"
      | Serial_big -> Fmt.string f "BIGSERIAL"
      | Text -> Fmt.string f "TEXT"
      | Time -> Fmt.string f "TIMESTAMP"
      | Varchar n -> Fmt.pf f "VARCHAR(%d)" n

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

type 't ast_const = {
  type_ : 't Type.t; [@printer Fmt.nop]
  value : 't; [@printer Fmt.nop]
}
[@@deriving show]

let rec equal_ast_const : type left right.
    left ast_const -> right ast_const -> bool =
 fun lhs rhs ->
  match (lhs, rhs) with
  | { type_ = Bool; value = lhs }, { type_ = Bool; value = rhs } ->
    Bool.equal lhs rhs
  | { type_ = Bigint; value = lhs }, { type_ = Bigint; value = rhs } ->
    Int64.equal lhs rhs
  | { type_ = Date; value = lhs }, { type_ = Date; value = rhs } ->
    Timmy.Date.compare lhs rhs = 0
  | { type_ = Float; value = lhs }, { type_ = Float; value = rhs } ->
    Float.equal lhs rhs
  | { type_ = Int; value = lhs }, { type_ = Int; value = rhs } -> lhs = rhs
  | { type_ = Json; value = lhs }, { type_ = Json; value = rhs } ->
    Poly.equal lhs rhs
  | { type_ = Null; _ }, { type_ = Null; _ } -> true
  | { type_ = Nullable _; value = None }, { type_ = Nullable _; value = None }
    ->
    true
  | ( { type_ = Nullable t; value = Some lhs },
      { type_ = Nullable t'; value = Some rhs } ) ->
    equal_ast_const { type_ = t; value = lhs } { type_ = t'; value = rhs }
  | { type_ = String; value = lhs }, { type_ = String; value = rhs } ->
    String.equal lhs rhs
  | { type_ = Time; value = lhs }, { type_ = Time; value = rhs } ->
    Timmy.Time.compare lhs rhs = 0
  | { type_ = Array _; _ }, _
  | { type_ = Bigint; _ }, _
  | { type_ = Bool; _ }, _
  | { type_ = Custom _; _ }, _
  | { type_ = Date; _ }, _
  | { type_ = Float; _ }, _
  | { type_ = Int; _ }, _
  | { type_ = Json; _ }, _
  | { type_ = Null; _ }, _
  | { type_ = Nullable _; _ }, _
  | { type_ = String; _ }, _
  | { type_ = Time; _ }, _
  | { type_ = []; _ }, _
  | { type_ = _ :: _; _ }, _ ->
    false

type ast =
  | Cast : {
      expr : ast;
      type' : _ Column.Type.atomic;
          [@printer Column.Type.pp_atomic] [@equal Column.Type.equal_atomic]
    }
      -> ast
  | Column of {
      selectable : string;
      names : string list;
    }
  | Const : (_ ast_const[@equal equal_ast_const]) -> ast
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
  | Now
  | Null
  | Select of {
      for' : for' option;
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
      type_ : _ Type.t; [@equal fun _ _ -> true] [@printer Type.pp]
    }
      -> ast

and for' =
  | Key_share
  | No_key_update
  | Share
  | Update
  | Update_of of string list

and join_type =
  | Inner
  | Outer
  | Outer_left
  | Outer_right
[@@deriving eq, show]

module type SELECTABLE = sig
  type model

  (** Type identifier of columns that can be selected from this *)
  type tag

  val to_ast : unit -> ast
end

module For = struct
  type 'tag t =
    | Key_share
    | No_key_update
    | Share
    | Update
    | Update_of of 'tag Selected.t list
end
