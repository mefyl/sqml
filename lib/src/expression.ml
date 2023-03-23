open Utils
include Expression_intf

module Column = struct
  include Column

  module Type = struct
    include Type

    let rec equal_atomic : type l r. l atomic -> r atomic -> bool =
     fun l r ->
      match (l, r) with
      | Bigint, Bigint
      | Bool, Bool
      | Date, Date
      | Float, Float
      | Int, Int
      | Json, Json
      | Text, Text
      | Time, Time ->
        true
      | Custom { repr; _ }, r -> equal_atomic repr r
      | l, Custom { repr; _ } -> equal_atomic l repr
      | Varchar l, Varchar r -> l = r
      | _ -> false

    let rec equal : type l r. l t -> r t -> bool =
     fun l r ->
      match (l, r) with
      | Atomic l, Atomic r -> equal_atomic l r
      | Composite { columns = l; _ }, Composite { columns = r; _ } ->
        equal_list (l, r)
      | _ -> false

    and equal_list : type l r. l List.t * r List.t -> bool = function
      | List.[], List.[] -> true
      | (l, hd_l) :: tl_l, (r, hd_r) :: tl_r ->
        String.(l = r) && equal_atomic hd_l hd_r && equal_list (tl_l, tl_r)
      | _ -> false

    let equal_any (Type l) (Type r) = equal l r

    let equal_any_atomic (Type l : any_atomic) (Type r : any_atomic) =
      equal_atomic l r
  end

  type ('t, 'tag) t = {
    name : string;
    selectable : string;
    type_ : 't Type.t;
  }

  module List = Hlist.Make2 (struct
    type nonrec ('t, 'tag) t = ('t, 'tag) t
  end)
end

module Type = struct
  include Type
  module Transduce_type = Column.Type.List.Transduce (Type.List)

  let rec of_atomic_column_type : type a. a Column.Type.atomic -> a t = function
    | Bigint -> Bigint
    | Bool -> Bool
    | Custom { encode; decode; repr } ->
      Custom { encode; decode; repr = of_atomic_column_type repr }
    | Date -> Date
    | Float -> Float
    | Int -> Int
    | Json -> Json
    | Nullable t -> Nullable (of_atomic_column_type t)
    | Text -> String
    | Time -> Time
    | Varchar _ -> String

  let of_column_type : type a. a Column.Type.t -> a t = function
    | Atomic t -> of_atomic_column_type t
    | Composite { columns; decode; encode } ->
      let types =
        Transduce_type.map columns
          ~f:{ f = (fun (_, t) -> of_atomic_column_type t) }
      in
      let rec conv : type a. a Type.List.t -> a Hlist.t t = function
        | [] -> []
        | hd :: tl -> hd :: conv tl
      in
      Custom
        { decode = Fn.compose Result.return decode; encode; repr = conv types }

  let rec pp : type a. a t Fmt.t =
    let open Fmt in
    fun f -> function
      | Array t -> (const char '[' ++ pp ++ const char ']') f t
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

module Traverse_ast (M : Monad.S2) = struct
  open Let.Syntax2 (M)

  let traverse_ast ast ~f =
    let rec traverse_ast = function
      | (Column _ | Const _ | Null | Selectable _ | Var _) as ast -> f ast
      | Funcall ({ args; _ } as pl) ->
        let+ args = Acid.List.map ~f:traverse_ast args |> M.all in
        Funcall { pl with args }
      | Infix ({ lhs; rhs; _ } as pl) ->
        let+ lhs = traverse_ast lhs
        and+ rhs = traverse_ast rhs in
        Infix { pl with lhs; rhs }
      | Is_null ast ->
        let+ ast = traverse_ast ast in
        Is_null ast
      | Is_not_null ast ->
        let+ ast = traverse_ast ast in
        Is_not_null ast
      | Join join ->
        let* left = traverse_ast join.left
        and* right = traverse_ast join.right in
        let+?? on = Option.map ~f:traverse_ast join.on in
        Join { join with left; right; on }
      | List elts ->
        let+ elts = Acid.List.map ~f:traverse_ast elts |> M.all in
        List elts
      | Select select ->
        let* from = traverse_ast select.from
        and* what = traverse_ast select.what in
        let+?? where = Option.map ~f:traverse_ast select.where in
        Select { select with from; what; where }
      | Switch { cases; default } ->
        let* cases =
          let f (cond, v) =
            let+ cond = traverse_ast cond
            and+ v = traverse_ast v in
            (cond, v)
          in
          Acid.List.map ~f cases |> M.all
        in
        let+?? default = Option.map ~f:traverse_ast default in
        Switch { cases; default }
    in
    traverse_ast ast
end

include Traverse_ast (struct
  type ('a, 'b) t = 'a

  include Monad.Make2 (struct
    type nonrec ('a, 'b) t = 'a

    let bind a ~f = f a
    let return a = a
    let map = `Custom (fun a ~f -> f a)
  end)
end)

let to_ast (_, ast) = ast
let to_type (t, _) = t

type ('t, 'tag) t = 't Type.t * ast

type ('t, 'tag) option =
  | None : (unit, 'tag) option
  | Some : ('t, 'tag) t -> ('t, 'tag) option

module List = Hlist.Make2 (struct
  type nonrec ('t, 'tag) t = ('t, 'tag) t
end)

module List_nullable = Hlist.Make2 (struct
  type nonrec ('t, 'tag) t = ('t Option.t, 'tag) t
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
      | hd :: tl ->
        (hd, Var { index = index + i; size = 1; type_ = hd }) :: f (i + 1) tl
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
  | Array _ | Bigint | Bool | Date | Float | Int | Json | Null | String | Time
    ->
    1

let prepare (type args) (types : args Type.List.t) : (args, 'free) List.t =
  let f index t =
    let size = type_arity t in
    (index + size, (t, Var { index; size; type_ = t }))
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
let make_column ~name ~selectable type_ = Column.{ name; selectable; type_ }

let coalesce = function
  | [] -> null
  | hd :: _ as args ->
    let args = Base.List.map ~f:snd args in
    (to_type hd, Funcall { args; name = "COALESCE" })

let coalesce_default args default =
  let args = Base.List.map ~f:snd args in
  (to_type default, Funcall { args = args @ [ snd default ]; name = "COALESCE" })

let atomic_columns = function
  | { Column.name; type_ = Atomic _; _ } -> [ name ]
  | { name; type_ = Composite { columns; _ }; _ } ->
    Column.Type.List.map columns ~f:{ f = (fun (c, _) -> name ^ "_" ^ c) }

let column (Column.{ selectable; type_; _ } as c) =
  (Type.of_column_type type_, Column { names = atomic_columns c; selectable })

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

let assignment_to_asts (Set { column; value }) =
  match column with
  | { Column.name; type_ = Atomic _; _ } -> [ (name, snd value) ]
  | { name; type_ = Composite { columns; _ }; _ } ->
    Column.Type.List.map columns
      ~f:{ f = (fun (c, _) -> (name ^ "_" ^ c, snd value)) }

module Infix = struct
  let arithmetics precedence op (_, lhs) (_, rhs) =
    (Type.Int, Infix { op; lhs; rhs; precedence })

  let logic precedence op (_, lhs) (_, rhs) =
    (Type.Bool, Infix { op; lhs; rhs; precedence })

  let ( && ) lhs rhs = logic 1 "AND" lhs rhs
  let ( := ) column value = Set { column; value }
  let ( < ) lhs rhs = logic 2 "<" lhs rhs
  let ( <= ) lhs rhs = logic 2 "<=" lhs rhs
  let ( = ) lhs rhs = logic 2 "=" lhs rhs
  let ( > ) lhs rhs = logic 2 ">" lhs rhs
  let ( >= ) lhs rhs = logic 2 ">=" lhs rhs

  let ( ^ ) (_, lhs) (_, rhs) =
    (Type.String, Infix { op = "||"; lhs; rhs; precedence = 2 })

  let ( || ) = logic 0 "OR"
  let ( + ) lhs rhs = arithmetics 3 "+" lhs rhs
  let ( - ) lhs rhs = arithmetics 3 "-" lhs rhs
end

include Infix

let _select (type tag) ~from:(module From : SELECTABLE with type tag = tag)
    ?for_ ?limit ?(skip_locked = false) ?where what =
  Select
    {
      for_;
      from = From.to_ast ();
      limit;
      skip_locked;
      what = to_ast what;
      where = Option.map ~f:to_ast where;
    }

let select ~from ?for_ ?limit ?skip_locked ?where what =
  (Type.Array (fst what), _select ~from ?for_ ?limit ?skip_locked ?where what)

let select_one ~from ?for_ ?skip_locked ?where what =
  ( Type.Nullable (fst what),
    _select ~from ?for_ ~limit:1 ?skip_locked ?where what )

let coerce = Fn.id
