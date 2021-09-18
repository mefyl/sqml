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
  | Int : int -> (int, 'tag) t
  | Logic : (logic * (bool, 'tag) t * (bool, 'tag) t) -> (bool, 'tag) t
  | Map : {
      decode : 'mapped -> 't;
      encode : 't -> 'mapped;
      mapped : ('mapped, 'tag) t;
    }
      -> ('t, 'tag) t
  | String : string -> (string, 'tag) t
  | Unit : (unit, 'tag) t
  | Var : (_, 'tag) t
  | ( :: ) :
      ('a, 'tag) t * ('b H_list.t, 'tag) t
      -> (('a -> 'b) H_list.t, 'tag) t
  | [] : (unit H_list.t, 'tag) t

module List = S_list.Make2 (struct
  type nonrec ('t, 'tag) t = ('t, 'tag) t
end)

let null = Unit

let var () = Var

let make_column ~selectable ~name type_ = { selectable; name; type_ }

let ( @ ) a b = a :: b

let empty_list = []

let quote_pattern = String.Search_pattern.create "'"

let rec pp : type a. Format.formatter -> (a, 'tag) t -> unit =
 fun f ->
  let open Fmt in
  function
  | Column { selectable; name; _ } ->
    (const string selectable ++ const string "." ++ const string name) f ()
  | Comparison (op, l, r) ->
    let fmt = const pp l ++ sp ++ pp_operator op ++ sp ++ const pp r in
    fmt f ()
  | Int i -> Fmt.int f i
  | Logic (op, l, r) ->
    let fmt = const pp l ++ sp ++ pp_logic op ++ sp ++ const pp r in
    fmt f ()
  | Map { mapped; _ } -> pp f mapped
  | [ a ] -> pp f a
  | String s ->
    let fmt = const char '\'' ++ string ++ const char '\'' in
    fmt f (String.Search_pattern.replace_all quote_pattern ~in_:s ~with_:"''")
  | Unit -> Fmt.string f "NULL"
  | Var -> Fmt.string f "?"
  | a :: b -> (const pp a ++ const string ", " ++ const pp b) f ()
  | [] -> ()

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

let ( = ) l r = Comparison (Eq, l, r)

let ( && ) l r = Logic (And, l, r)

let ( || ) l r = Logic (And, l, r)

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
  | Int _ as e -> e
  | Logic (op, l, r) -> Logic (op, map_name f l, map_name f r)
  | Map ({ mapped; _ } as pl) -> Map { pl with mapped = map_name f mapped }
  | String _ as e -> e
  | Unit as e -> e
  | Var as e -> e
  | a :: b -> map_name f a :: map_name f b
  | [] as e -> e
