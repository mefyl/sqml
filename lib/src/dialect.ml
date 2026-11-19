open Acid
include Dialect_intf

let rec column_type_of_expression_type : type t.
    t Expression.Type.t -> Expression.Column.Type.any_atomic option = function
  | Array t ->
    Option.map (column_type_of_expression_type t) ~f:(fun (Type atomic) ->
        (Type (Array atomic) : Expression.Column.Type.any_atomic))
  | Bigint -> Some (Type Bigint)
  | Bool -> Some (Type Bool)
  | Date -> Some (Type Date)
  | Float -> Some (Type Float)
  | Int -> Some (Type Int)
  | Json -> Some (Type Json)
  | Null -> None
  | Nullable t -> column_type_of_expression_type t
  | String -> Some (Type Text)
  | Time -> Some (Type Time)
  | Custom { repr; _ } -> column_type_of_expression_type repr
  | [] | _ :: _ -> None

let rec json_of_sqml : type t. t Expression.Type.t -> t -> Ezjsonm.value =
  function
  | Array elements ->
    let json_of_element = json_of_sqml elements in
    fun list -> `A (List.map ~f:json_of_element list)
  | Bool -> fun bool -> `Bool bool
  | Custom { repr; encode; _ } ->
    let json_of_repr = json_of_sqml repr in
    fun value -> json_of_repr (encode value)
  | Int -> fun int -> `Float (Float.of_int int)
  | String -> fun string -> `String string
  | type' ->
    Fmt.failwith "json_of_sqml: unsupported type %a" Expression.Type.pp type'

let rec sqml_of_json : type t.
    t Expression.Type.t -> Ezjsonm.value -> (t, string) Result.t =
 fun type' ->
  let fail value =
    Result.fail
    @@ Fmt.str "not a valid %a: %s" Expression.Type.pp type'
    @@ Ezjsonm.value_to_string value
  in
  match type' with
  | Array elements -> (
    let element_of_json = sqml_of_json elements in
    function
    | `A list -> List.map ~f:element_of_json list |> Result.all
    | value -> fail value)
  | Bool -> ( function `Bool bool -> Result.Ok bool | value -> fail value)
  | Custom { repr; decode; _ } ->
    let open Let.Syntax2 (Result) in
    let repr_of_json = sqml_of_json repr in
    fun json ->
      let* repr = repr_of_json json in
      decode repr
  | Int -> (
    function
    | `Float float -> Result.Ok (Float.to_int float)
    | value -> fail value)
  | String -> (
    function `String string -> Result.Ok string | value -> fail value)
  | type' ->
    Fmt.failwith "sqml_of_json: unsupported type %a" Expression.Type.pp type'

let pp_literal_string =
  let quote_pattern = String.Search_pattern.create "'" in
  fun f value ->
    let open Fmt in
    let fmt = const char '\'' ++ string ++ const char '\'' in
    fmt f
      (String.Search_pattern.replace_all quote_pattern ~in_:value ~with_:"''")

module Make (D : sig
  val schemas : bool
  val pp_array : 'element Expression.Type.t -> 'a Fmt.t -> 'a Fmt.t
  val string_of_type : _ Expression.Column.Type.atomic -> string

  (** The current timestamp as RFC3339. *)
  val now : string

  (** [pp_in ~negated ~value ~set] tests [value] for membership in [set]. *)
  val pp_in : negated:bool -> value:unit Fmt.t -> set:unit Fmt.t -> unit Fmt.t
end) =
struct
  include D

  let string_of_type ?(include_nullable = true) t =
    let type_ = string_of_type t in
    if (not include_nullable) || Expression.Column.Type.nullable t then type_
    else Fmt.str "%s NOT NULL" type_

  let pp_type fmt = Fmt.of_to_string (string_of_type ~include_nullable:true) fmt

  let rec pp_from_type : type t. t Expression.Type.t -> t Fmt.t =
    let open Fmt in
    let pp_time fmt t =
      char fmt '\'';
      string fmt (Timmy.Time.to_rfc3339 t);
      char fmt '\''
    in
    let rec pp_values : type l. l Hlist.t Expression.Type.t -> l Hlist.t Fmt.t =
      function
      | [] -> nop
      | [ hd ] -> fun f [ hd_v ] -> pp_from_type hd f hd_v
      | hd :: tl ->
        fun f (hd_v :: tl_v) ->
          pp_from_type hd f hd_v;
          comma f ();
          pp_values tl f tl_v
      | Custom { repr; encode; _ } -> fun f v -> pp_from_type repr f @@ encode v
      | Time ->
        (* Timmy.Time.t being abstract, the typer cannot prove it's not
           unifiable with [l Hlist.t], so we must handle it. *)
        pp_time
    in
    function
    | Array element ->
      Fmt.using
        (Fn.compose Ezjsonm.value_to_string @@ json_of_sqml (Array element))
      @@ D.pp_array element (const char '\'' ++ string ++ const char '\'')
    | Bigint -> Fmt.int64
    | Bool -> Fmt.bool
    | Date -> const char '\'' ++ Timmy.Date.pp ++ const char '\''
    | Float -> Fmt.float
    | Int -> Fmt.int
    | Json -> using Ezjsonm.(value_to_string ~minify:true) pp_literal_string
    | Null -> Fmt.(const string "NULL")
    | Nullable t -> Fmt.(option ~none:(const string "NULL") @@ pp_from_type t)
    | String -> pp_literal_string
    | Time -> pp_time
    | Custom { encode; repr; _ } ->
      fun f value -> pp_from_type repr f @@ encode value
    | [] -> nop
    | _ :: _ as l -> pp_values l

  let rec pp_ast ?(context = Default) f ast =
    let open Fmt in
    let var (type t) (type_ : t Expression.Type.t) =
      match type_ with
      | Array element -> pp_array element (const char '$' ++ using Int.succ int)
      | type_ -> (
        let type_ =
          type_
          |> column_type_of_expression_type
          |> Option.map ~f:(fun (Type t : Expression.Column.Type.any_atomic) ->
              string_of_type ~include_nullable:false t)
        in
        match type_ with
        | None -> const char '$' ++ using Int.succ int
        | Some type_ ->
          const string "CAST($"
          ++ using Int.succ int
          ++ const string " AS "
          ++ const string type_
          ++ const char ')')
    in
    let rec pp current_precedence : Expression.ast Fmt.t =
     fun f -> function
       | Cast { expr; type' } ->
         (const string "CAST("
         ++ const (pp None) expr
         ++ const string " AS "
         ++ const string (string_of_type ~include_nullable:false type')
         ++ const char ')')
           f ()
       | Column { names; _ } when equal_context context Index_definition ->
         Fmt.list ~sep:comma string f names
       | Column { selectable; names; _ } ->
         Fmt.list ~sep:comma
           (const string selectable ++ const string "." ++ string)
           f names
       | Const { type_; value } -> pp_from_type type_ f value
       | Default -> Fmt.string f "DEFAULT"
       | Funcall { name; args; _ } ->
         (const string name
         ++ sp
         ++ const char '('
         ++ list ~sep:comma (pp None)
         ++ const char ')')
           f args
       | Infix { op; lhs; rhs; precedence } -> (
         let fmt =
           const (pp (Some precedence)) lhs
           ++ sp
           ++ const string op
           ++ sp
           ++ const
                (if precedence = -1 then
                   const char '(' ++ pp (Some precedence) ++ const char ')'
                 else pp (Some precedence))
                rhs
         in
         match current_precedence with
         | Some current_precedence when current_precedence > precedence ->
           (const char '(' ++ fmt ++ const char ')') f ()
         | _ -> fmt f ())
       | In { value; set; negated } ->
         D.pp_in ~negated
           ~value:(fun f () -> pp None f value)
           ~set:(fun f () -> pp None f set)
           f ()
       | Is_null ast -> (pp None ++ sp ++ const string "IS NULL") f ast
       | Is_not_null ast -> (pp None ++ sp ++ const string "IS NOT NULL") f ast
       | Join { left; right; on; type_ } ->
         let verb =
           match type_ with
           | Inner -> "INNER JOIN"
           | Outer -> "OUTER JOIN"
           | Outer_left -> "LEFT OUTER JOIN"
           | Outer_right -> "RIGHT OUTER JOIN"
         in
         (const (pp None) left
         ++ sp
         ++ const string verb
         ++ sp
         ++ const (pp None) right
         ++ sp
         ++ const string "ON"
         ++ sp
         ++ const (option ~none:(const string "true") pp_ast) on)
           f ()
       | List l -> list ~sep:comma (pp None) f l
       | Now -> Fmt.string f D.now
       | Null -> Fmt.string f "NULL"
       | Select { for'; from; group_by; limit; skip_locked; what; where } ->
         pf f "(@,@[<2>SELECT %a@] @[<2>FROM %a@]%a%a%a%a%a)" (pp None) what
           (pp None) from
           (option (sp ++ box ~indent:2 (const string "WHERE " ++ pp None)))
           where
           (option (sp ++ box ~indent:2 (const string "GROUP BY " ++ pp None)))
           group_by
           (option (sp ++ const string "FOR " ++ pp_for))
           for'
           (option (sp ++ string))
           (if skip_locked then Some "SKIP LOCKED" else None)
           (option (sp ++ pp_limit))
           limit
       | Selectable { name; alias } ->
         (const string name ++ const (option (const char ' ' ++ string)) alias)
           f ()
       | Star -> const string "*" f ()
       | Switch { cases; default } ->
         let pp_case f (cond, value) =
           (const string "WHEN"
           ++ sp
           ++ const (pp None) cond
           ++ sp
           ++ const string "THEN"
           ++ sp
           ++ const (pp None) value
           ++ sp)
             f ()
         in
         (const string "CASE"
         ++ sp
         ++ const (list pp_case) cases
         ++ const (option (const string "ELSE" ++ sp ++ pp None ++ sp)) default
         ++ const string "END")
           f ()
       | Var { index; size; type_ } ->
         (list ~sep:comma (var type_)) f @@ List.range index (index + size)
    in
    pp None f ast

  and pp_limit = Fmt.(const string "LIMIT " ++ int)

  and pp_for f = function
    | Expression.Key_share -> Fmt.string f "KEY SHARE"
    | No_key_update -> Fmt.string f "NO KEY UPDATE"
    | Share -> Fmt.string f "SHARE"
    | Update | Update_of [] -> Fmt.string f "UPDATE"
    | Update_of tables ->
      Fmt.(pf f "UPDATE OF %a" (list ~sep:comma string) tables)

  let pp = pp_ast
end

let string_of_type_postgres t =
  let rec f : type t. t Expression.Column.Type.atomic -> string = function
    | Array element -> f element ^ "[]"
    | Bigint -> "BIGINT"
    | Bool -> "BOOLEAN"
    | Custom { repr; _ } -> f repr
    | Date -> "DATE"
    | Float -> "REAL"
    | Int -> "INTEGER"
    | Json -> "JSONB"
    | Nullable t -> f t
    | Serial -> "SERIAL"
    | Serial_big -> "BIGSERIAL"
    | Text -> "TEXT"
    | Time -> "TIMESTAMP"
    | Varchar len -> Fmt.str "VARCHAR(%i)" len
  in
  f t

module Postgresql : S = Make (struct
  let schemas = true

  let pp_array element pp =
    let rec element_type : type element.
        element Expression.Type.t -> (_, _, _, _) format4 = function
      | Custom { repr; _ } -> element_type repr
      | Int -> "::int"
      | String -> ""
      | type' ->
        Fmt.failwith "unsupported array elements: %a" Expression.Type.pp type'
    in
    fun f ->
      Fmt.fmt
        Stdlib.(
          "(ARRAY (SELECT value"
          ^^ element_type element
          ^^ " FROM json_array_elements_text(%a) AS value))")
        f pp

  let string_of_type = string_of_type_postgres
  let now = "NOW()"

  let pp_in ~negated ~value ~set f () =
    Fmt.pf f "%a %s (%a)" value ()
      (if negated then "!= ALL" else "= ANY")
      set ()
end)

let postgresql = (module Postgresql : S)

module Sqlite : S = Make (struct
  let schemas = false
  let pp_array _ pp f = Fmt.fmt "json(%a)" f pp

  let string_of_type t =
    let rec f : type t. t Expression.Column.Type.atomic -> string = function
      | Custom { repr; _ } -> f repr
      | Date | Json | Time -> "TEXT"
      | Nullable t -> f t
      | t -> string_of_type_postgres t
    in
    f t

  let now = "strftime('%Y-%m-%d %H:%M:%f+00:00', 'now')"

  (* SQLite has no array type: membership is a join on the JSON document. *)
  let pp_in ~negated ~value ~set f () =
    Fmt.pf f "%a %s (SELECT value FROM json_each(%a))" value ()
      (if negated then "NOT IN" else "IN")
      set ()
end)

let sqlite = (module Sqlite : S)
