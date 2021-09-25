open Acid
include Expression_intf

module Column = struct
  module Type = struct
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
      | Nullable : 't t -> 't option t
      | Text : string t
      | Time : Timmy.Time.t t
      | Varchar : int -> string t
  end

  type ('t, 'tag) t = {
    selectable : string;
    name : string;
    type_ : 't Type.t;
  }

  module List = Hlist.Make2 (struct
    type nonrec ('t, 'tag) t = ('t, 'tag) t
  end)
end

type free = unit

module Type = struct
  type 'a t =
    | Bool : bool t
    | Date : Timmy.Date.t t
    | Int : int t
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
    | Bool -> Bool
    | Custom { encode; decode; repr } ->
      Custom { encode; decode; repr = of_column_type repr }
    | Date -> Date
    | Int -> Int
    | Nullable t -> Nullable (of_column_type t)
    | Text -> String
    | Time -> Time
    | Varchar _ -> String

  let rec pp : type a. a t Fmt.t =
    let open Fmt in
    fun f -> function
      | Bool -> Fmt.string f "bool"
      | Custom { repr; _ } -> pp f repr
      | Date -> Fmt.string f "date"
      | Int -> Fmt.string f "int"
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
  | List : ast list -> ast
  | Null : ast
  | Switch : {
      cases : (ast * ast) list;
      default : ast option;
    }
      -> ast
  | Var : 't Type.t -> ast

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

let const type_ value = (type_, Const { type_; value })

let null = (Type.Null, Null)

let var t = (t, Var t)

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

let make_column ~selectable ~name type_ = Column.{ selectable; name; type_ }

let coalesce = function
  | [] -> null
  | hd :: _ as args ->
    let args = Base.List.map ~f:snd args in
    (to_type hd, Funcall { args; name = "COALESCE" })

let coalesce_default args default =
  let args = Base.List.map ~f:snd args in
  (to_type default, Funcall { args = args @ [ snd default ]; name = "COALESCE" })

let column Column.{ name; selectable; type_ } =
  (Type.of_column_type type_, Column { name; selectable })

let map ~decode ~encode (type_, v) =
  let type_ = Type.Custom { decode; encode; repr = type_ } in

  (type_, v)

let map_name f (t, v) =
  let rec map_name = function
    | Column ({ selectable; _ } as pl) ->
      Column { pl with selectable = f selectable }
    | Infix ({ lhs; rhs; _ } as pl) ->
      Infix { pl with lhs = map_name lhs; rhs = map_name rhs }
    | Const _ as e -> e
    | Funcall ({ args; _ } as pl) ->
      Funcall { pl with args = Base.List.map ~f:map_name args }
    | List l -> List (Base.List.map ~f:map_name l)
    | Switch { cases; default } ->
      Switch
        {
          cases =
            Base.List.map
              ~f:(fun (cond, value) -> (map_name cond, map_name value))
              cases;
          default = Option.map ~f:map_name default;
        }
    | Null as e -> e
    | Var _ as e -> e
  in
  (t, map_name v)

type 'tag assignment =
  | Set : {
      column : ('t, 'tag) Column.t;
      value : ('t, 'tag) t;
    }
      -> 'tag assignment

let assignment_to_ast (Set { column; value = _, ast }) = (column.name, ast)

module Infix = struct
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
end

include Infix
