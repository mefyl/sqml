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

type ('t, 'tag) t =
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
  | Null : ('t option, 'tag) t
  | String : string -> (string, 'tag) t
  | Var : (_, 'tag) t
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

let null = Null

let var () = Var

let make_column ~selectable ~name type_ = { selectable; name; type_ }

let empty_list = []

let quote_pattern = String.Search_pattern.create "'"

let rec pp : type a. Format.formatter -> (a, 'tag) t -> unit =
 fun f ->
  let open Fmt in
  function
  | Column { selectable; name; _ } ->
    (const string selectable ++ const string "." ++ const string name) f ()
  | Concat (l, r) ->
    let fmt = const pp l ++ sp ++ const string "||" ++ sp ++ const pp r in
    fmt f ()
  | Comparison (op, l, r) ->
    let fmt = const pp l ++ sp ++ pp_operator op ++ sp ++ const pp r in
    fmt f ()
  | Int i -> Fmt.int f i
  | Logic (op, l, r) ->
    let fmt = const pp l ++ sp ++ pp_logic op ++ sp ++ const pp r in
    fmt f ()
  | Map { mapped; _ } -> pp f mapped
  | Null -> Fmt.string f "NULL"
  | String s ->
    let fmt = const char '\'' ++ string ++ const char '\'' in
    fmt f (String.Search_pattern.replace_all quote_pattern ~in_:s ~with_:"''")
  | Var -> Fmt.string f "?"
  | [] -> ()
  | [ a ] -> pp f a
  | a :: b -> (const pp a ++ const string ", " ++ const pp b) f ()

and pp_operator =
  let open Fmt in
  function
  | Eq -> const char '='
  | Gt -> const char '>'
  | Gte -> const string ">="
  | Lt -> const char '<'
  | Lte -> const string "<="

and pp_logic =
  let open Fmt in
  function
  | And -> const string "AND"
  | Or -> const string "OR"

let column c = Column c

let int i = Int i

let map ~decode ~encode mapped = Map { decode; encode; mapped }

let string s = String s

let rec map_name :
    type a tag tag'. (string -> string) -> (a, tag) t -> (a, tag') t =
 fun f -> function
  | Column ({ selectable; _ } as pl) ->
    Column { pl with selectable = f selectable }
  | Comparison (op, l, r) -> Comparison (op, map_name f l, map_name f r)
  | Concat (l, r) -> Concat (map_name f l, map_name f r)
  | Int _ as e -> e
  | Logic (op, l, r) -> Logic (op, map_name f l, map_name f r)
  | Map ({ mapped; _ } as pl) -> Map { pl with mapped = map_name f mapped }
  | String _ as e -> e
  | Null as e -> e
  | Var as e -> e
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

let rec to_type : type a. (a, _) t -> a Type.extended = function
  | Column { type_; _ } -> Atomic type_
  | Comparison _ -> Atomic Bool
  | Concat _ -> Atomic Text
  | Int _ -> Atomic Int
  | Logic _ -> Atomic Bool
  | Map { mapped; encode; decode } ->
    Custom
      {
        decode = (fun x -> Result.Ok (decode x));
        encode = (fun x -> Result.Ok (encode x));
        repr = to_type mapped;
      }
  | String _ -> Atomic Text
  | hd :: tl -> Pair (to_type hd, to_type tl)
  | [] -> Empty
  | Null -> failwith "can't deduce type of NULL"
  | Var -> failwith "prepared query of variable type not supported"
