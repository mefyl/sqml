open Acid
include Expression_intf

type ('a, 'tag) t =
  | Column : {
      selectable : string;
      column : 't Column.t;
    }
      -> ('t, 'tag) t
  | Pair : ('a, 'tag) t * ('b, 'tag) t -> ('a * 'b, 'tag) t
  | Comparison : (comparison * ('t, 'tag) t * ('t, 'tag) t) -> (bool, 'tag) t
  | Int : int -> (int, 'tag) t
  | String : string -> (string, 'tag) t
  | Var : (_, 'tag) t

let var () = Var

let make_column selectable column = Column { selectable; column }

let ( @ ) a b = Pair (a, b)

let quote_pattern = String.Search_pattern.create "'"

let rec pp : type a. Format.formatter -> (a, 'tag) t -> unit =
 fun f ->
  let open Fmt in
  function
  | Column { selectable; column } ->
    (const string selectable ++ const string "." ++ const string column.name)
      f ()
  | Comparison (op, l, r) ->
    let fmt = const pp l ++ sp ++ pp_operator op ++ sp ++ const pp r in
    fmt f ()
  | Pair (a, b) -> (const pp a ++ const string ", " ++ const pp b) f ()
  | Int i -> Fmt.int f i
  | String s ->
    let fmt = const char '\'' ++ string ++ const char '\'' in
    fmt f (String.Search_pattern.replace_all quote_pattern ~in_:s ~with_:"''")
  | Var -> Fmt.string f "?"

and pp_operator =
  let open Fmt in
  function
  | Eq -> const char '='
  | Gt -> const char '>'
  | Gte -> const string ">="
  | Lt -> const char '<'
  | Lte -> const string "<="

let ( = ) l r = Comparison (Eq, l, r)

let int i = Int i

let string s = String s

let rec map_name :
    type a tag tag'. (string -> string) -> (a, tag) t -> (a, tag') t =
 fun f -> function
  | Column { selectable; column } ->
    Column { selectable = f selectable; column }
  | Comparison (op, l, r) -> Comparison (op, map_name f l, map_name f r)
  | Pair (a, b) -> Pair (map_name f a, map_name f b)
  | Int _ as e -> e
  | String _ as e -> e
  | Var as e -> e
