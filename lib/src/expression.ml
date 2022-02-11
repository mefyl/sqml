open Utils
include Expression_intf

module Column = struct
  module Type = struct
    type 'a t =
      | Bigint : Int64.t t
      | Bool : bool t
      | Custom : {
          repr : 'repr t;
          encode : 'a -> 'repr;
          decode : 'repr -> ('a, string) Result.t;
        }
          -> 'a t
      | Date : Timmy.Date.t t
      | Float : float t
      | Int : int t
      | Json : Ezjsonm.value t
      | Nullable : 't t -> 't option t
      | Text : string t
      | Time : Timmy.Time.t t
      | Varchar : int -> string t
  end

  type ('t, 'tag) t = {
    name : string;
    primary : bool;
    selectable : string;
    type_ : 't Type.t;
  }

  module List = Hlist.Make2 (struct
    type nonrec ('t, 'tag) t = ('t, 'tag) t
  end)
end

module Type = struct
  type 'a t =
    | Bigint : Int64.t t
    | Bool : bool t
    | Date : Timmy.Date.t t
    | Float : float t
    | Int : int t
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
    | ( :: ) : 'a t * 'b Hlist.t t -> ('a, 'b) Hlist.desc Hlist.t t

  module List = Hlist.Make (struct
    type nonrec 't t = 't t
  end)

  let rec of_column_type : type a. a Column.Type.t -> a t = function
    | Bigint -> Bigint
    | Bool -> Bool
    | Custom { encode; decode; repr } ->
      Custom { encode; decode; repr = of_column_type repr }
    | Date -> Date
    | Float -> Float
    | Int -> Int
    | Json -> Json
    | Nullable t -> Nullable (of_column_type t)
    | Text -> String
    | Time -> Time
    | Varchar _ -> String

  let rec pp : type a. a t Fmt.t =
    let open Fmt in
    fun f -> function
      | Bigint -> Fmt.string f "bigint"
      | Bool -> Fmt.string f "bool"
      | Custom { repr; _ } -> pp f repr
      | Float -> Fmt.string f "float"
      | Date -> Fmt.string f "date"
      | Int -> Fmt.string f "int"
      | Json -> Fmt.string f "JSON"
      | Nullable t -> (pp ++ const string "?") f t
      | Null -> Fmt.string f "null"
      | String -> Fmt.string f "string"
      | Time -> Fmt.string f "time"
      | [] -> Fmt.string f "[]"
      | hd :: tl ->
        pp f hd;
        Fmt.string f " :: ";
        pp f tl
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
      default : ast option;
    }
      -> ast
  | Var : {
      index : int;
      size : int;
    }
      -> ast

let traverse_ast ast ~f =
  let rec traverse_ast = function
    | (Column _ | Const _ | Null | Var _) as ast -> f ast
    | Funcall ({ args; _ } as pl) ->
      Funcall { pl with args = List.map ~f:traverse_ast args }
    | Infix ({ lhs; rhs; _ } as pl) ->
      Infix { pl with lhs = traverse_ast lhs; rhs = traverse_ast rhs }
    | Is_null ast -> Is_null (traverse_ast ast)
    | Is_not_null ast -> Is_not_null (traverse_ast ast)
    | List elts -> List (List.map ~f:traverse_ast elts)
    | Switch { cases; default } ->
      Switch
        {
          cases =
            List.map
              ~f:(fun (cond, v) -> (traverse_ast cond, traverse_ast v))
              cases;
          default = Option.map ~f:traverse_ast default;
        }
  in
  traverse_ast ast

let to_ast (_, ast) = ast
let to_type (t, _) = t

type ('t, 'tag) t = 't Type.t * ast

type ('t, 'tag) option =
  | None : (unit, 'tag) option
  | Some : ('t, 'tag) t -> ('t, 'tag) option

module List = Hlist.Make2 (struct
  type nonrec ('t, 'tag) t = ('t, 'tag) t
end)

let of_list l =
  let type_ =
    let rec of_exps : type l tag. (l, tag) List.t -> l Hlist.t Type.t = function
      | List.[] -> Type.[]
      | List.(hd :: tl) -> Type.(to_type hd :: of_exps tl)
    in
    of_exps l
  and v =
    let rec of_exps : type l tag. (l, tag) List.t -> ast list = function
      | List.[] -> []
      | List.(hd :: tl) -> to_ast hd :: of_exps tl
    in
    of_exps l
  in
  (type_, List v)

module Transduce_exprs = Type.List.Transduce2 (List)

(* This one looks a bit flaky, but we actually guarantee only this type/ast
   combination is valid. We could preserve a strong typing on the AST, but it
   would greatly complexify the code just for this specific case. *)
let to_list = function
  | type_, List value ->
    let rec f : type a. a Hlist.t Type.t * _ -> (a, 'tag) List.t = function
      | hd_t :: tl_t, hd :: tl -> (hd_t, hd) :: f (tl_t, tl)
      | [], [] -> []
      | _ -> failwith "unreachable B"
    in
    f (type_, value)
  | type_, Var { index; _ } ->
    let rec f : type a. int -> a Hlist.t Type.t -> (a, 'tag) List.t =
     fun i -> function
      | hd :: tl -> (hd, Var { index = index + i; size = 1 }) :: f (i + 1) tl
      | [] -> []
      | _ -> failwith "unreachable C"
    in
    f 0 type_
  | _ -> Fmt.failwith "unreachable A"

let const type_ value = (type_, Const { type_; value })
let null = (Type.Null, Null)
let is_null (_, v) = (Type.Bool, Is_null v)
let is_not_null (_, v) = (Type.Bool, Is_not_null v)
let some (t, v) = (Type.Nullable t, v)

let rec type_arity : type t. t Type.t -> int = function
  | Custom { repr; _ } -> type_arity repr
  | [] -> 0
  | hd :: tl -> type_arity hd + type_arity tl
  | Nullable t -> type_arity t
  | Bigint | Bool | Date | Float | Int | Json | Null | String | Time -> 1

let prepare (type args) (types : args Type.List.t) : (args, 'free) List.t =
  let f index t =
    let size = type_arity t in
    (index + size, (t, Var { index; size }))
  in
  Transduce_exprs.folding_map ~f:{ f } 0 types

let case = function
  | [] -> null
  | (_, hd) :: _ as cases ->
    let type_ = to_type hd
    and ast =
      Switch
        {
          cases =
            Base.List.map
              ~f:(fun (cond, value) -> (to_ast cond, to_ast value))
              cases;
          default = None;
        }
    in
    (type_, ast)

let case_else cases default =
  let type_ = to_type default
  and ast =
    Switch
      {
        cases =
          Base.List.map
            ~f:(fun (cond, value) -> (to_ast cond, to_ast value))
            cases;
        default = Some (to_ast default);
      }
  in
  (type_, ast)

let now = (Type.Time, Funcall { name = "NOW"; args = [] })

let make_column ~name ~primary ~selectable type_ =
  Column.{ name; primary; selectable; type_ }

let coalesce = function
  | [] -> null
  | hd :: _ as args ->
    let args = Base.List.map ~f:snd args in
    (to_type hd, Funcall { args; name = "COALESCE" })

let coalesce_default args default =
  let args = Base.List.map ~f:snd args in
  (to_type default, Funcall { args = args @ [ snd default ]; name = "COALESCE" })

let column Column.{ name; selectable; type_; _ } =
  (Type.of_column_type type_, Column { name; selectable })

let map ~decode ~encode (type_, v) =
  let type_ = Type.Custom { decode; encode; repr = type_ } in

  (type_, v)

let map_name f (t, v) =
  let f = function
    | Column ({ selectable; _ } as pl) ->
      Column { pl with selectable = f selectable }
    | ast -> ast
  in
  (t, traverse_ast ~f v)

type 'tag assignment =
  | Set : {
      column : ('t, 'tag) Column.t;
      value : ('t, 'tag) t;
    }
      -> 'tag assignment

let assignment_to_ast (Set { column; value = _, ast }) = (column.name, ast)

module Infix = struct
  let arithmetics op (_, lhs) (_, rhs) = (Type.Int, Infix { op; lhs; rhs })
  let logic op (_, lhs) (_, rhs) = (Type.Bool, Infix { op; lhs; rhs })
  let ( && ) lhs rhs = logic "AND" lhs rhs
  let ( := ) column value = Set { column; value }
  let ( < ) lhs rhs = logic "<" lhs rhs
  let ( <= ) lhs rhs = logic "<=" lhs rhs
  let ( = ) lhs rhs = logic "=" lhs rhs
  let ( > ) lhs rhs = logic ">" lhs rhs
  let ( >= ) lhs rhs = logic ">=" lhs rhs
  let ( ^ ) (_, lhs) (_, rhs) = (Type.String, Infix { op = "||"; lhs; rhs })
  let ( || ) = logic "OR"
  let ( + ) lhs rhs = arithmetics "+" lhs rhs
  let ( - ) lhs rhs = arithmetics "-" lhs rhs
end

include Infix

let coerce = Fn.id
