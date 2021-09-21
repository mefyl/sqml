open Acid
include Expression_intf

type ('t, 'tag) column = {
  selectable : string;
  name : string;
  type_ : 't Type.t;
}

module Column_list = S_list.Make2 (struct
  type ('t, 'tag) t = ('t, 'tag) column
end)

type free = unit

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
  | Empty : unit H_list.t type_
  | Pair : 'a type_ * 'b H_list.t type_ -> ('a -> 'b) H_list.t type_

type ('t, 'tag) t =
  | Column : ('t, 'tag) column -> ('t, 'tag) t
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
  | Null : ('t option, 'tag) t
  | Var : 't type_ -> ('t, 'tag) t
  | ( :: ) :
      ('a, 'tag) t * ('b H_list.t, 'tag) t
      -> (('a -> 'b) H_list.t, 'tag) t
  | [] : (unit H_list.t, 'tag) t

type ('t, 'tag) option =
  | None : (unit, 'tag) option
  | Some : ('t, 'tag) t -> ('t, 'tag) option

module List = S_list.Make2 (struct
  type nonrec ('t, 'tag) t = ('t, 'tag) t
end)

let const type_ value = Const { type_; value }

let null = Null

let var t = Var t

let make_column ~selectable ~name type_ = { selectable; name; type_ }

let empty_list = []

(* let quote_pattern = String.Search_pattern.create "'"
 *
 * let rec pp : type a. Format.formatter -> (a, 'tag) t -> unit =
 *  fun f ->
 *   let open Fmt in
 *   function
 *   | Column { selectable; name; _ } ->
 *     (const string selectable ++ const string "." ++ const string name) f ()
 *   | Concat (l, r) ->
 *     let fmt = const pp l ++ sp ++ const string "||" ++ sp ++ const pp r in
 *     fmt f ()
 *   | Comparison (op, l, r) ->
 *     let fmt = const pp l ++ sp ++ pp_operator op ++ sp ++ const pp r in
 *     fmt f ()
 *   | Const _ -> Fmt.string f "<CONST>"
 *   | Int i -> Fmt.int f i
 *   | Logic (op, l, r) ->
 *     let fmt = const pp l ++ sp ++ pp_logic op ++ sp ++ const pp r in
 *     fmt f ()
 *   | Map { mapped; _ } -> pp f mapped
 *   | Null -> Fmt.string f "NULL"
 *   | String s ->
 *     let fmt = const char '\'' ++ string ++ const char '\'' in
 *     fmt f (String.Search_pattern.replace_all quote_pattern ~in_:s ~with_:"''")
 *   | Var -> Fmt.string f "?"
 *   | [] -> ()
 *   | [ a ] -> pp f a
 *   | a :: b -> (const pp a ++ const string ", " ++ const pp b) f ()
 *
 * and pp_operator =
 *   let open Fmt in
 *   function
 *   | Eq -> const char '='
 *   | Gt -> const char '>'
 *   | Gte -> const string ">="
 *   | Lt -> const char '<'
 *   | Lte -> const string "<="
 *
 * and pp_logic =
 *   let open Fmt in
 *   function
 *   | And -> const string "AND"
 *   | Or -> const string "OR" *)

let column c = Column c

let int = Int

let map ~decode ~encode mapped = Map { decode; encode; mapped }

let string = String

let some t = Nullable t

let rec map_name :
    type a tag tag'. (string -> string) -> (a, tag) t -> (a, tag') t =
 fun f -> function
  | Column ({ selectable; _ } as pl) ->
    Column { pl with selectable = f selectable }
  | Comparison (op, l, r) -> Comparison (op, map_name f l, map_name f r)
  | Concat (l, r) -> Concat (map_name f l, map_name f r)
  | Const _ as e -> e
  | Logic (op, l, r) -> Logic (op, map_name f l, map_name f r)
  | Map ({ mapped; _ } as pl) -> Map { pl with mapped = map_name f mapped }
  | Null as e -> e
  | Var _ as e -> e
  | a :: b -> map_name f a :: map_name f b
  | [] as e -> e

type 'tag assignment =
  | Set : {
      column : ('t, 'tag) column;
      value : ('t, 'tag) t;
    }
      -> 'tag assignment

module Infix = struct
  let ( && ) l r = Logic (And, l, r)

  let ( := ) column value = Set { column; value }

  let ( = ) l r = Comparison (Eq, l, r)

  let ( @ ) a b = a :: b

  let ( ^ ) a b = Concat (a, b)

  let ( || ) l r = Logic (And, l, r)
end

include Infix
