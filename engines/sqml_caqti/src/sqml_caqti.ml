open Sqml
open Utils
module Array = Array.Monadic2 (Lwt_result)
open Let.Syntax (Lwt)

let rec json_of_sqml : type t. t Sqml.Expression.Type.t -> t -> Ezjsonm.value =
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
  | _ -> failwith "fixme json"

let rec sqml_of_json : type t.
    t Sqml.Expression.Type.t -> Ezjsonm.value -> (t, string) Result.t =
 fun type' ->
  let fail value =
    Result.fail
    @@ Fmt.str "not a valid %a: %s" Sqml.Expression.Type.pp type'
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
  | _ -> failwith "fixme json"

let rec caqti_of_type : type t. t Sqml.Expression.Type.t -> t Caqti_type.t =
  function
  | Array _ as t ->
    let to_json = json_of_sqml t
    and of_json = sqml_of_json t in
    let encode value =
      Result.return (Ezjsonm.value_to_string ~minify:true @@ to_json value)
    and decode string =
      let open Let.Syntax2 (Result) in
      let* json =
        try Result.return (Ezjsonm.value_from_string string)
        with Ezjsonm.Parse_error (_, error) -> Result.fail error
      in
      of_json json
    in
    Caqti_type.(custom ~encode ~decode string)
  | Bigint -> Caqti_type.int64
  | Bool -> Caqti_type.bool
  | Date ->
    let encode d =
      Timmy.Date.to_time ~timezone:Timmy.Timezone.utc d
      |> Timmy.Time.to_ptime
      |> Result.return
    and decode t =
      Timmy.Time.of_ptime t
      |> Timmy.Date.of_time ~timezone:Timmy.Timezone.utc
      |> Result.return
    in
    Caqti_type.custom ~decode ~encode @@ Caqti_type.pdate
  | Float -> Caqti_type.float
  | Int -> Caqti_type.int
  | Json ->
    let encode json = Result.return (Ezjsonm.value_to_string ~minify:true json)
    and decode string =
      try Result.return (Ezjsonm.value_from_string string)
      with Ezjsonm.Parse_error (_, error) -> Result.fail error
    in
    Caqti_type.(custom ~encode ~decode string)
  | Null ->
    (* We don't have `Caqti.never`, but this should be safe since it can never
       return `Some _` *)
    Caqti_type.(option (Stdlib.Obj.magic unit))
  | Nullable t -> Caqti_type.option (caqti_of_type t)
  | String -> Caqti_type.string
  | Time ->
    let encode t = Result.return @@ Timmy.Time.to_ptime t
    and decode t = Result.return @@ Timmy.Time.of_ptime t in
    Caqti_type.custom ~decode ~encode @@ Caqti_type.ptime
  | Custom { encode; decode; repr } ->
    Caqti_type.custom ~decode ~encode:(fun v -> Result.return @@ encode v)
    @@ caqti_of_type repr
  | [] ->
    let encode Hlist.[] = Result.return ()
    and decode () = Result.return Hlist.[] in
    Caqti_type.custom ~decode ~encode @@ Caqti_type.unit
  | l :: r ->
    let encode Hlist.(hd :: tl) = Result.return @@ (hd, tl)
    and decode (hd, tl) = Result.return @@ Hlist.(hd :: tl) in
    Caqti_type.custom ~decode ~encode
    @@ Caqti_type.t2 (caqti_of_type l) (caqti_of_type r)

let rec caqti_of_types : type l.
    l Sqml.Expression.Type.List.t -> l Hlist.t Caqti_type.t =
  let open Caqti_type in
  function
  | hd :: tl ->
    custom
      ~decode:(fun (hd, tl) -> Result.Ok Hlist.(hd :: tl))
      ~encode:(fun Hlist.(hd :: tl) -> Result.Ok (hd, tl))
    @@ t2 (caqti_of_type hd) (caqti_of_types tl)
  | [] ->
    custom
      ~decode:(fun () -> Result.Ok Hlist.[])
      ~encode:(fun Hlist.[] -> Result.Ok ())
    @@ unit

let type_of_expr e = Sqml.Expression.to_type e |> caqti_of_type

module Make (Model : Model.S) : sig
  include Engine.S with module Model = Model

  type current_query

  val current_query : transaction -> current_query option
  val pp_current_query : current_query Fmt.t

  val with_savepoint :
    pp_error:'error Fmt.t ->
    transaction ->
    ?tags:Logs.Tag.set ->
    (transaction -> ('result, ([> Engine.error ] as 'error)) Lwt_result.t) ->
    ('result, 'error) Lwt_result.t

  val make :
    ?max_size:int ->
    ?post_creation_statements:(Caqti_driver_info.dialect_tag -> string list) ->
    string ->
    (t, [> Engine.error ]) Lwt_result.t
end = struct
  type t = {
    dialect : Sqml.Dialect.t;
    pool : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t;
  }

  let dialect { dialect; _ } = dialect

  type transaction = {
    connection : (module Caqti_lwt.CONNECTION) Lwt_guarded.t;
    current_query : current_query option ref;
    mutable error : Sqml.Engine.error Option.t;
  }

  and current_query = Query : _ Model.Query.t -> current_query

  let quote_pattern = String.Search_pattern.create "'"

  let pp_literal_string f value =
    let open Fmt in
    let fmt = const char '\'' ++ string ++ const char '\'' in
    fmt f
      (String.Search_pattern.replace_all quote_pattern ~in_:value ~with_:"''")

  let rec column_type_of_expression_type : type t.
      t Sqml.Expression.Type.t -> Sqml.Expression.Column.Type.any_atomic option
      = function
    | Array _ -> None
    | Bigint -> Some (Type Sqml.Expression.Column.Type.Bigint)
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

  let table_name info schema name =
    match schema with
    | None -> Fmt.str "%S" name
    | Some schema -> (
      match Caqti_driver_info.dialect_tag info with
      | `Sqlite -> Fmt.str "%s_%s" (String.escaped schema) (String.escaped name)
      | `Pgsql -> Fmt.str "%S.%S" schema name
      | _ -> failwith "dialect not handled")

  let fail fmt = Fmt.kstr (fun s -> Lwt_result.fail (`Sqml_engine s)) fmt

  let convert_error =
    let f = function
      | `Request_failed
          ({
             msg =
               Caqti_driver_postgresql.Result_error_msg
                 { sqlstate = "40P01"; _ };
             _;
           } :
            Caqti_error.query_error) ->
        `Sqml_deadlock
      | `Request_failed
          ({
             msg =
               Caqti_driver_postgresql.Result_error_msg
                 { sqlstate = "23502"; _ };
             _;
           } :
            Caqti_error.query_error) ->
        `Not_null_violation
      | `Request_failed
          ({
             msg =
               Caqti_driver_postgresql.Result_error_msg
                 { sqlstate = "23503"; _ };
             _;
           } :
            Caqti_error.query_error) ->
        `Foreign_key_violation
      | `Request_failed
          ({
             msg =
               Caqti_driver_postgresql.Result_error_msg
                 { sqlstate = "23505"; error_message };
             _;
           } :
            Caqti_error.query_error) ->
        `Unique_violation error_message
      | e -> `Sqml_engine (Caqti_error.show e)
    in
    Result.map_error ~f

  let error = Lwt.map ~f:convert_error

  type 'res column_mapper = { f : 't. 't Sqml.Column.t -> 'res option }

  let column_map (f : 'res column_mapper) =
    let rec map : type t. t Column.List.t -> 'res list = function
      | Column.List.(column :: tl) -> (
        match f.f column with Some value -> value :: map tl | None -> map tl)
      | [] -> []
    in
    map

  let make ?max_size ?(post_creation_statements = fun _ -> []) uri =
    let open Caqti_request.Infix in
    let*! pool =
      let config = Caqti_pool_config.create ?max_size () in
      Caqti_lwt_unix.connect_pool ~pool_config:config (Uri.of_string uri)
      |> Lwt.return
      |> error
    in
    let create (module C : Caqti_lwt.CONNECTION) =
      let dialect, found_query =
        match Caqti_driver_info.dialect_tag C.driver_info with
        | `Sqlite ->
          ( Sqml.Dialect.sqlite,
            (Caqti_type.unit ->! Caqti_type.bool)
              "SELECT COUNT(*) > 0 FROM sqlite_master WHERE type='table' AND \
               name= 'sqml_metadata'" )
        | `Pgsql ->
          ( Sqml.Dialect.postgresql,
            (Caqti_type.unit ->! Caqti_type.bool)
              "SELECT EXISTS (SELECT FROM information_schema.tables WHERE \
               table_schema = 'sqml' AND table_name = 'metadata')" )
        | _ -> failwith "dialect not handled"
      in
      if not @@ Caqti_driver_info.can_transact C.driver_info then
        Lwt_result.return (None, dialect)
      else
        let*! found = C.find found_query () in
        let caqti_metadata_type = Caqti_type.(t4 int int int ptime) in
        if found then
          let query =
            (Caqti_type.unit ->! caqti_metadata_type)
              (Fmt.str
                 "SELECT major, minor, subminor, migrated FROM %s ORDER BY %s \
                  DESC LIMIT 1"
                 (table_name C.driver_info (Some "sqml") "metadata")
                 (if
                    Poly.(Caqti_driver_info.dialect_tag C.driver_info = `Sqlite)
                  then "datetime(migrated)"
                  else "migrated"))
          in
          let*! major, minor, subminor, _ = C.find query () in
          let current = Semver.from_parts major minor subminor [] [] in
          let* () =
            Logs.info (fun m ->
                m "load model at version %a" Fmt.(option Semver.pp) current)
          in
          Lwt_result.return (current, dialect)
        else
          let* () =
            Logs.info (fun m ->
                m "create model at version %s" @@ Semver.to_string Model.version)
          in
          (* Create tables and related indexes *)
          let create_table (type tag) (module T : Table.S with type tag = tag) =
            let*! () =
              match Caqti_driver_info.dialect_tag C.driver_info with
              | `Sqlite -> Lwt_result.return ()
              | `Pgsql -> (
                match T.Meta.schema with
                | None -> Lwt_result.return ()
                | Some schema ->
                  let query =
                    (Caqti_type.unit ->. Caqti_type.unit)
                      ~oneshot:true
                      (Fmt.str "CREATE SCHEMA IF NOT EXISTS %S" schema)
                  in
                  C.exec query ())
              | _ -> failwith "dialect not handled"
            in
            let queries = Sqml.Table.Query.create (module T) in
            let* () =
              Logs.debug (fun m ->
                  m "create table:@ %a"
                    Fmt.(
                      list
                        ~sep:(const char ';' ++ sp)
                        (Sqml.Table.Query.pp dialect))
                    queries)
            in
            let exec query =
              let query =
                (Caqti_type.unit ->. Caqti_type.unit)
                  ~oneshot:true
                  (Fmt.to_to_string (Sqml.Table.Query.pp dialect) query)
              in
              C.exec query ()
            in
            Lwt_list.map_s exec queries |> Lwt.map ~f:Result.all_unit
          in
          let*! () = Array.iter ~f:create_table (Model.tables ()) in
          (* Apply post-creation statements *)
          let*! () =
            let post_creation_statements =
              post_creation_statements
              @@ Caqti_driver_info.dialect_tag C.driver_info
            in
            let* () =
              Logs.debug (fun m ->
                  m "run post-creation statements:@ %a"
                    Fmt.(list ~sep:(const char ';' ++ sp) string)
                    post_creation_statements)
            in
            let exec query =
              let query =
                (Caqti_type.unit ->. Caqti_type.unit) ~oneshot:true query
              in
              C.exec query ()
            in
            post_creation_statements
            |> Lwt_list.map_s exec
            |> Lwt.map ~f:Result.all_unit
          in

          let query =
            (caqti_metadata_type ->. Caqti_type.unit)
              (Fmt.str
                 "INSERT INTO %s (major, minor, subminor, migrated) VALUES (?, \
                  ?, ?, ?)"
                 (table_name C.driver_info (Some "sqml") "metadata"))
          in
          let*! () =
            let Semver.{ major; minor; patch; _ } = Model.version
            and now = Option.value_exn (Ptime.of_float_s @@ Unix.time ()) in
            C.exec query (major, minor, patch, now)
          in
          Lwt_result.return (Some Model.version, dialect)
    in
    Caqti_lwt_unix.Pool.use create pool |> error >>=! function
    | Some current, dialect ->
      if Poly.(current <> Model.version) then
        fail "model version mismatch: code at %s, data at %s"
          (Semver.to_string Model.version)
          (Semver.to_string current)
      else Lwt_result.return { dialect; pool }
    | None, _ -> fail "backend does not support transactions"

  let close { pool; _ } = Caqti_lwt_unix.Pool.drain pool

  module Model = Model

  let query_type : type t arity. (t, arity) Model.Query.t -> t Caqti_type.t =
    function
    | Delete _ -> Caqti_type.unit
    | Delete_returning { returning; _ } -> type_of_expr returning
    | Delete_unique { returning; _ } -> type_of_expr returning
    | Insert _ -> Caqti_type.unit
    | Insert_returning { returning; _ } -> type_of_expr returning
    | Select (Select { what; _ }) -> type_of_expr what
    | Select_unique (Select { what; _ }) -> type_of_expr what
    | Update _ -> Caqti_type.unit
    | Update_returning { returning; _ } -> type_of_expr returning
    | Update_unique { returning; _ } -> type_of_expr returning

  let dialect_of_dialect_tag = function
    | #Caqti_driver_info.sql_dialect_tag as t -> t
    | _ -> `Other

  let pp_array (type element) dialect (element : element Sqml.Expression.Type.t)
      pp =
    match dialect with
    | `Pgsql ->
      let rec element_type : type element.
          element Sqml.Expression.Type.t -> (_, _, _, _) format4 = function
        | Custom { repr; _ } -> element_type repr
        | Int -> "::int"
        | String -> ""
        | type' ->
          Fmt.failwith "unsupported array elements: %a" Sqml.Expression.Type.pp
            type'
      in
      fun f ->
        Fmt.fmt
          Stdlib.(
            "(ARRAY (SELECT value"
            ^^ element_type element
            ^^ " FROM json_array_elements_text(%a) AS value))")
          f pp
    | `Sqlite -> fun f -> Fmt.fmt "json('%a')" f pp
    | `Mysql | `Other -> failwith "array not supported yet"

  let pp_from_type dialect =
    let rec pp_from_type : type t. t Sqml.Expression.Type.t -> t Fmt.t =
      let open Fmt in
      let pp_time fmt t =
        char fmt '\'';
        string fmt (Timmy.Time.to_rfc3339 t);
        char fmt '\''
      in
      let rec pp_values : type l.
          l Hlist.t Sqml.Expression.Type.t -> l Hlist.t Fmt.t = function
        | [] -> nop
        | [ hd ] -> fun f [ hd_v ] -> pp_from_type hd f hd_v
        | hd :: tl ->
          fun f (hd_v :: tl_v) ->
            pp_from_type hd f hd_v;
            comma f ();
            pp_values tl f tl_v
        | Custom { repr; encode; _ } ->
          fun f v -> pp_from_type repr f @@ encode v
        | Time ->
          (* Timmy.Time.t being abstract, the typer cannot prove it's not
             unifiable with [l Hlist.t], so we must handle it. *)
          pp_time
      in
      function
      | Array element ->
        Fmt.using
          (Fn.compose Ezjsonm.value_to_string @@ json_of_sqml (Array element))
        @@ pp_array dialect element
             (const char '\'' ++ string ++ const char '\'')
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
    in
    pp_from_type

  let pp_query dialect =
    let rec pp_expression f ast =
      let open Fmt in
      let var (type t) (type_ : t Sqml.Expression.Type.t) =
        match type_ with
        | Array element ->
          pp_array dialect element (const char '$' ++ using Int.succ int)
        | type_ -> (
          let type_ =
            type_
            |> column_type_of_expression_type
            |> Option.map
                 ~f:(fun (Type t : Sqml.Expression.Column.Type.any_atomic) ->
                   Sqml.Dialect.Postgresql.string_of_type
                     ~include_nullable:false t)
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
         | Column { selectable; names; _ } ->
           Fmt.list ~sep:comma
             (const string selectable ++ const string "." ++ string)
             f names
         | Const { type_; value } -> pp_from_type dialect type_ f value
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
         | Is_null ast -> (pp None ++ sp ++ const string "IS NULL") f ast
         | Is_not_null ast ->
           (pp None ++ sp ++ const string "IS NOT NULL") f ast
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
           ++ const (option ~none:(const string "true") pp_expression) on)
             f ()
         | List l -> list ~sep:comma (pp None) f l
         | Null -> Fmt.string f "NULL"
         | Select { for'; from; group_by; limit; skip_locked; what; where } ->
           pf f "(@,@[<2>SELECT %a@] @[<2>FROM %a@]%a%a%a%a%a)" (pp None) what
             (pp None) from
             (option (sp ++ box ~indent:2 (const string "WHERE " ++ pp None)))
             where
             (option
                (sp ++ box ~indent:2 (const string "GROUP BY " ++ pp None)))
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
           ++ const
                (option (const string "ELSE" ++ sp ++ pp None ++ sp))
                default
           ++ const string "END")
             f ()
         | Var { index; size; type_ } ->
           (list ~sep:comma (var type_)) f @@ List.range index (index + size)
      in
      pp None f ast
    and pp_for f = function
      | Expression.Share -> Fmt.string f "SHARE"
      | Update | Update_of [] -> Fmt.string f "UPDATE"
      | Update_of tables ->
        Fmt.(pf f "UPDATE OF %a" (list ~sep:comma string) tables)
    and pp_limit = Fmt.(const string "LIMIT " ++ int) in
    let pp : type tag arity. (tag, arity) Model.Query.t Fmt.t =
     fun f ->
      let open Fmt in
      let pp_clause keyword f e = Fmt.pf f "%s %a" keyword pp_expression e in
      let pp_assignment f =
        Fmt.(
          using Expression.assignment_to_asts
          @@ list ~sep:comma
          @@ pair ~sep:(sp ++ const char '=' ++ sp) string pp_expression)
          f
      and pp_returning : type t tag. (t, tag) Expression.option Fmt.t =
       fun f -> function
         | Expression.None -> ()
         | Some e ->
           Fmt.(
             (sp
             ++ box ~indent:2 (const string "RETURNING" ++ sp ++ pp_expression)
             )
               f (Expression.to_ast e))
      in
      let pp_delete (type t tag)
          (from :
            (module Table.S with type model = Model.tag and type tag = tag))
          (where : (bool, tag) Expression.t option)
          (returning : (t, tag) Expression.t option) =
        let module Table = (val from) in
        Fmt.pf f "@[<2>DELETE FROM@ %a@]%a%a" pp_expression (Table.to_ast ())
          (option (sp ++ box ~indent:2 (pp_clause "WHERE")))
          (Option.map ~f:Expression.to_ast where)
          (option (sp ++ box ~indent:2 (pp_clause "RETURNING")))
          (Option.map ~f:Expression.to_ast returning)
      and pp_insert (type arity mapped res tag)
          (module T : Table.S
            with type model = Model.tag
             and type tag = tag
             and type Meta.mapped = mapped)
          ({ excluded } : tag Sqml.Model.excluded)
          (on_conflict : (arity, tag) Sqml.Model.on_conflict)
          (returning : (res, tag) Expression.option) value =
        let primary =
          let (Unique { columns; _ }) = T.primary in
          Table.Index.Column.List.map
            ~f:{ f = (fun { column = { name; _ }; _ } -> name) }
            columns
        in
        let conflict_sqlite, conflict_pg =
          let open Fmt in
          match (dialect, on_conflict) with
          | _, Fail -> (nop, nop)
          | (`Pgsql | `Other), Ignore ->
            (nop, const string " ON CONFLICT DO NOTHING")
          | (`Pgsql | `Sqlite | `Other), Set assignments ->
            ( nop,
              const string " ON CONFLICT ("
              ++ const (list ~sep:comma string) primary
              ++ const string ") DO UPDATE SET "
              ++ const
                   (list ~sep:comma pp_assignment)
                   (assignments { excluded }) )
          | (`Pgsql | `Other), Update ->
            let f { Column.name; type_; _ } =
              let rec pp_columns : 't. 't Column.Type.t -> string list =
               fun (type t) (t : t Column.Type.t) ->
                match t with
                | Column.Type.Atomic _ ->
                  [ Fmt.str "%s = excluded.%s" name name ]
                | Composite { columns; _ } ->
                  Column.Type.List.map columns
                    ~f:
                      {
                        f =
                          (fun (c, _) ->
                            Fmt.str "%s_%s = excluded.%s_%s" name c name c);
                      }
                | Custom { repr; _ } -> pp_columns repr
                | Nullable t -> pp_columns t
              in
              Some (pp_columns type_)
            in
            ( nop,
              const string " ON CONFLICT ("
              ++ const (list ~sep:comma string) primary
              ++ const string ") DO UPDATE SET "
              ++ const (list ~sep:comma string)
                   (column_map { f } T.Meta.columns |> List.concat) )
          | `Sqlite, Ignore -> (const string " OR IGNORE", nop)
          | `Sqlite, Update -> (const string " OR REPLACE", nop)
          | `Mysql, _ -> failwith "dialect not handled"
        in
        (* Explicitely enumerate columns order on insertion, migration that add
           columns subsequently mess up the column ordering. *)
        Fmt.(
          pf f "INSERT%a INTO %S (%a) VALUES(%a)%a%a" conflict_sqlite ()
            T.Meta.name (list ~sep:comma string)
            (Table.Column.List.map
               ~f:
                 {
                   f =
                     (fun { name; type_; _ } ->
                       Sqml.Expression.Column.Type.atomic_columns name type_);
                 }
               T.Meta.columns
            |> List.concat)
            pp_expression value conflict_pg () pp_returning returning)
      and pp_select (type t)
          (Select
             { for'; from; group_by; limit; order_by; skip_locked; what; where } :
            t Model.Query.select) =
        let pp_for f =
          let forable =
            let rec forable =
              let outer (name, _) = (name, false) in
              function
              | Expression.Selectable { alias; name } ->
                [ (Option.value ~default:name alias, true) ]
              | Join { left; right; type_ = Inner; _ } ->
                forable left @ forable right
              | Join { left; right; type_ = Outer_left; _ } ->
                forable left @ List.map ~f:outer @@ forable right
              | Join { left; right; type_ = Outer_right; _ } ->
                (List.map ~f:outer @@ forable left) @ forable right
              | Join { left; right; type_ = Outer; _ } ->
                (List.map ~f:outer @@ forable left)
                @ List.map ~f:outer
                @@ forable right
              | _ -> []
            in
            let module From = (val from) in
            forable (From.to_ast ())
          and pp_forable f forable =
            if List.for_all ~f:snd forable then ()
            else
              Fmt.pf f " OF %a" (list ~sep:comma string)
              @@ List.filter_map
                   ~f:(fun (name, forable) -> Option.some_if forable name)
                   forable
          in
          function
          | Sqml.Expression.Share ->
            Fmt.(const string "FOR SHARE" ++ pp_forable) f forable
          | Update | Update_of [] ->
            Fmt.(const string "FOR UPDATE" ++ pp_forable) f forable
          | Update_of tables ->
            Fmt.(pf f "FOR UPDATE OF %a" (list ~sep:comma string) tables)
        and pp_order_by f =
          let pp_order_by (expression, ordering) =
            Fmt.pf f "%a %a"
              (using Expression.to_ast pp_expression)
              expression Table.pp_ordering ordering
          in
          function
          | Sqml.Model.Order_by.[] -> ()
          | head :: tail ->
            Fmt.pf f "@ ORDER BY ";
            pp_order_by head;
            let rec loop = function
              | Sqml.Model.Order_by.[] -> ()
              | head :: tail ->
                Fmt.comma f ();
                pp_order_by head;
                loop tail
            in
            loop tail
        in
        let module Selectable = (val from) in
        (box ~indent:2
           (const string "SELECT"
           ++ sp
           ++ const pp_expression Expression.(to_ast what))
        ++ sp
        ++ box ~indent:2
             (const string "FROM"
             ++ sp
             ++ const pp_expression (Selectable.to_ast ()))
        ++ const
             (option (sp ++ box ~indent:2 (pp_clause "WHERE")))
             Expression.(Option.map ~f:to_ast where)
        ++ const
             (option (sp ++ box ~indent:2 (pp_clause "GROUP BY")))
             Expression.(Option.map ~f:to_ast group_by)
        ++ const pp_order_by order_by
        ++ const (option (sp ++ pp_for)) for'
        ++ const
             (option (sp ++ string))
             (if skip_locked then Some "SKIP LOCKED" else None)
        ++ const (option (sp ++ pp_limit)) limit)
          f ()
      and pp_update (type res tag from)
          (module T : Table.S with type model = Model.tag and type tag = tag)
          (what : tag Expression.assignment list)
          (from :
            (module Selectable.S with type model = Model.tag and type tag = from)
            option) (where : (bool, tag) Expression.t option)
          (returning : (res, tag) Expression.option) =
        let from =
          match from with
          | None -> None
          | Some (module From) -> Some (From.to_ast ())
        in
        (box ~indent:2
           (const string "UPDATE" ++ sp ++ const pp_expression (T.to_ast ()))
        ++ sp
        ++ box ~indent:2
             (const
                (const string "SET" ++ sp ++ list ~sep:comma pp_assignment)
                what)
        ++ const
             (option
                (sp
                ++ box ~indent:2 (const string "FROM" ++ sp ++ pp_expression)))
             from
        ++ const
             (option
                (sp
                ++ box ~indent:2 (const string "WHERE" ++ sp ++ pp_expression)))
             Expression.(Option.map ~f:to_ast where)
        ++ const pp_returning returning)
          f ()
      in
      function
      | Delete { from; where } ->
        let module Table = (val from) in
        Fmt.pf f "DELETE FROM@ %a%a" pp_expression (Table.to_ast ())
          (option (sp ++ pp_clause "WHERE"))
          Expression.(Option.map ~f:to_ast where)
      | Delete_returning { from; returning; where } ->
        pp_delete from where (Some returning)
      | Delete_unique { from; returning; where } ->
        pp_delete from where (Some returning)
      | Insert { excluded; into; on_conflict; value } ->
        pp_insert into { excluded } on_conflict Expression.None
          Expression.(to_ast value)
      | Insert_returning { excluded; into; on_conflict; returning; value } ->
        pp_insert into { excluded } on_conflict (Some returning)
          Expression.(to_ast value)
      | Select_unique select -> pp_select select
      | Select select -> pp_select select
      | Update { into; what; where } ->
        pp_update into what None where Expression.None
      | Update_returning { from; into; returning; where; what } ->
        pp_update into what from where Expression.(Some returning)
      | Update_unique { from; into; returning; where; what } ->
        pp_update into what from where Expression.(Some returning)
    in
    pp

  let current_query { current_query; _ } = !current_query
  let pp_current_query f (Query q) = pp_query `Pgsql f q

  let acquire transaction ~f =
    let f connection =
      match transaction.error with
      | Some error ->
        Lwt_result.fail (error : Sqml.Engine.error :> [> Sqml.Engine.error ])
      | None -> (
        f connection >>| function
        | Result.Error (#Sqml.Engine.error as error) as result ->
          let () = transaction.error <- Some error in
          result
        | result -> result)
    in
    Lwt_guarded.acquire ~f transaction.connection

  let with_current_query transaction query f =
    let () = transaction.current_query := Some (Query query) in
    Lwt.finalize f (fun () -> Lwt.return (transaction.current_query := None))

  let query transaction ?tags (type res arity)
      (query : (res, arity) Model.Query.t) :
      ((res, arity, _) Sqml.Model.result, _) Lwt_result.t =
    let open Caqti_request.Infix in
    let f (module C : Caqti_lwt.CONNECTION) =
      let collect query =
        let f response =
          let f stream =
            Lwt.bind (stream ()) ~f:(function
              | Caqti_lwt.Stream.Nil -> Lwt_result.return None
              | Error e -> Lwt_result.fail e |> error
              | Cons (v, next) -> Lwt_result.return @@ Some (v, next))
          and init = C.Response.to_stream response in
          Lwt_result.return @@ Sqml.Model.Many (Sequence.unfold_step ~init ~f)
        in
        C.call ~f query ()
      in
      let dialect =
        dialect_of_dialect_tag @@ Caqti_driver_info.dialect_tag C.driver_info
      in
      let query_str = Fmt.str "%a" (pp_query dialect) query in
      let* () =
        Logs.debug @@ fun m ->
        m ?tags "execute query:@ @[%a@]" (pp_query dialect) query
      in
      let res : ((res, arity, _) Sqml.Model.result, _) Lwt_result.t =
        match query with
        | Delete _ ->
          let query = (Caqti_type.unit ->. Caqti_type.unit) query_str in
          let+! () = C.exec query () in
          Sqml.Model.Zero
        | Delete_returning { returning; _ } ->
          let query = (Caqti_type.unit ->* type_of_expr returning) query_str in
          collect query
        | Delete_unique { returning; _ } ->
          let query = (Caqti_type.unit ->? type_of_expr returning) query_str in
          let+! res = C.find_opt query () in
          Sqml.Model.One_maybe res
        | Select (Select { what; _ }) ->
          let query = (Caqti_type.unit ->* type_of_expr what) query_str in
          collect query
        | Select_unique (Select { what; _ }) ->
          let query = (Caqti_type.unit ->? type_of_expr what) query_str in
          let+! res = C.find_opt query () in
          Sqml.Model.One_maybe res
        | Insert _ ->
          let query = (Caqti_type.unit ->. Caqti_type.unit) query_str in
          let+! () = C.exec query () in
          Sqml.Model.Zero
        | Insert_returning { on_conflict; returning; _ } -> (
          let one () =
            let query =
              (Caqti_type.unit ->! type_of_expr returning) query_str
            in
            let+! res = C.find query () in
            Sqml.Model.One res
          and one_maybe () =
            let query =
              (Caqti_type.unit ->? type_of_expr returning) query_str
            in
            let+! res = C.find_opt query () in
            Sqml.Model.One_maybe res
          in
          match on_conflict with
          | Fail -> one ()
          | Ignore -> one_maybe ()
          | Set _ -> one ()
          | Update -> one ())
        | Update _ ->
          let query = (Caqti_type.unit ->. Caqti_type.unit) query_str in
          let+! () = C.exec query () in
          Sqml.Model.Zero
        | Update_returning { returning; _ } ->
          let query = (Caqti_type.unit ->* type_of_expr returning) query_str in
          collect query
        | Update_unique { returning; _ } ->
          let query = (Caqti_type.unit ->? type_of_expr returning) query_str in
          let+! res = C.find_opt query () in
          Sqml.Model.One_maybe res
      in
      res |> error
    in
    acquire ~f transaction

  module Hlist_printers = Hlist.Make (struct
    type 'a t = 'a Fmt.t
  end)

  module Printers_of_types = Sqml.Expression.Type.List.Transduce (Hlist_printers)

  let pp_values_of_types dialect types =
    let open Fmt in
    let printers =
      Printers_of_types.map ~f:{ f = (fun t -> pp_from_type dialect t) } types
    in
    let rec print : type l.
        l Hlist_printers.t -> Formatter.t -> l Hlist.t -> unit = function
      | [] -> fun _ [] -> ()
      | [ printer ] -> fun fmt [ value ] -> printer fmt value
      | printer :: printers ->
        let tail = print printers in
        fun fmt (value :: values) ->
          printer fmt value;
          Fmt.comma fmt ();
          tail fmt values
    in
    const string "[" ++ sp ++ box (print printers) ++ const string " ]"

  type ('args, 'tag, 'res, 'arity) prepared = {
    query : ('res, 'arity) Model.Query.t;
    prepared : ('args Hlist.t, 'res, 'arity) prepared_query;
    types : 'args Expression.Type.List.t;
  }

  and ('args, 'res, 'arity) prepared_query =
    | Zero :
        ('args, unit, [ `Zero ]) Caqti_request.t
        -> ('args, 'res, Sqml.Model.zero) prepared_query
    | One :
        ('args, 'res, [ `One ]) Caqti_request.t
        -> ('args, 'res, Sqml.Model.one) prepared_query
    | One_maybe :
        ('args, 'res, [ `One | `Zero ]) Caqti_request.t
        -> ('args, 'res, Sqml.Model.one_maybe) prepared_query
    | Many :
        ('args, 'res, [ `Many | `One | `Zero ]) Caqti_request.t
        -> ('args, 'res, Sqml.Model.many) prepared_query

  let pp_prepared fmt { query; _ } = pp_query `Pgsql fmt query

  let prepare (type args res arity) (types : args Expression.Type.List.t)
      (query : (args, 'tag) Expression.List.t -> (res, arity) Model.Query.t) :
      (args, _, res, arity) prepared =
    let query = query @@ Sqml.Expression.prepare types
    and args = caqti_of_types types in
    let prepare_str tag =
      let query_str dialect =
        let q = Fmt.str "%a" (pp_query dialect) query in
        let () =
          Logs.Sync.debug (fun m ->
              m "@[<2>prepare query:@ @[%a@]@]@ @[<hv 2>arguments:@ %a@]"
                (pp_query dialect) query
                (Expression.Type.List.pp ~sep:Fmt.comma
                   { f = Sqml.Expression.Type.pp })
                types)
        in
        q
      in
      query_str (dialect_of_dialect_tag tag)
    in
    let open Caqti_request.Infix in
    match Model.Query.arity query with
    | Zero ->
      let prepared = (args -->. Caqti_type.unit) @@:- prepare_str in
      { query; prepared = Zero prepared; types }
    | One ->
      let prepared = (args -->! query_type query) @@:- prepare_str in
      { query; prepared = One prepared; types }
    | One_maybe ->
      let prepared = (args -->? query_type query) @@:- prepare_str in
      { query; prepared = One_maybe prepared; types }
    | Many ->
      let prepared = (args -->* query_type query) @@:- prepare_str in
      { query; prepared = Many prepared; types }

  let run (type args res arity)
      ({ query; prepared; types } : (args, 'tag, res, arity) prepared)
      transaction ?tags (values : args Hlist.t) =
    let* () =
      match types with
      | [] ->
        Logs.debug (fun m ->
            m ?tags "@[<2>execute prepared query:@ @[%a@]@]" (pp_query `Pgsql)
              query)
      | types ->
        let pp_values = pp_values_of_types `Pgsql types in
        Logs.debug (fun m ->
            m ?tags
              "@[<2>execute prepared query:@ @[%a@]@]@ @[<hv 2>values: %a@]"
              (pp_query `Pgsql) query pp_values values)
    in
    match prepared with
    | Zero prepared ->
      let+! () =
        let f (module C : Caqti_lwt.CONNECTION) =
          with_current_query transaction query @@ fun () ->
          C.exec prepared values |> error
        in
        acquire ~f transaction
      in
      (Sqml.Model.Zero
        : (res, arity, ([> Sqml.Engine.error ] as 'error)) Sqml.Model.result)
    | One prepared ->
      let+! res =
        let f (module C : Caqti_lwt.CONNECTION) =
          with_current_query transaction query @@ fun () ->
          C.find prepared values |> error
        in
        acquire ~f transaction
      in
      Sqml.Model.One res
    | One_maybe prepared ->
      let+! res =
        let f (module C : Caqti_lwt.CONNECTION) =
          with_current_query transaction query @@ fun () ->
          C.find_opt prepared values |> error
        in
        acquire ~f transaction
      in
      Sqml.Model.One_maybe res
    | Many prepared ->
      let+! res =
        let f (module C : Caqti_lwt.CONNECTION) =
          with_current_query transaction query @@ fun () ->
          let f response =
            let f stream =
              Lwt.bind (stream ()) ~f:(function
                | Caqti_lwt.Stream.Nil -> Lwt_result.return None
                | Error e -> Lwt_result.fail e |> error
                | Cons (v, next) -> Lwt_result.return @@ Some (v, next))
            and init = C.Response.to_stream response in
            Lwt_result.return @@ Sequence.unfold_step ~init ~f
          in
          C.call ~f prepared values |> error
        in
        acquire ~f transaction
      in
      Sqml.Model.Many res

  let unmap = function
    | `Shitty_api e -> e
    | #Caqti_error.t as e -> `Sqml_engine (Caqti_error.show e)

  let connect t ?tags f =
    let* result =
      let f ((module C : Caqti_lwt.CONNECTION) as connection) =
        let* () = Logs.debug @@ fun m -> m ?tags "transaction start" in
        let*! () = C.start () in
        let f () =
          f
            {
              connection = Lwt_guarded.make connection;
              current_query = ref None;
              error = None;
            }
          >>= function
          | Result.Error e ->
            let* () = Logs.debug @@ fun m -> m ?tags "transaction rollback" in
            let*! () = C.rollback () in
            Lwt_result.fail (`Shitty_api e)
          | Result.Ok res ->
            let* () = Logs.debug @@ fun m -> m ?tags "transaction commit" in
            let+! () = C.commit () in
            res
        and catch exn =
          let* () = Logs.debug @@ fun m -> m ?tags "transaction rollback" in
          let*! () = C.rollback () in
          raise exn
        in
        Lwt.catch f catch
      in
      Caqti_lwt_unix.Pool.use f
        (t.pool
          : (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t
          :> (Caqti_lwt.connection, [> Caqti_error.t ]) Caqti_lwt_unix.Pool.t)
    in
    match result with
    | Result.Ok _ as res -> Lwt.return res
    | Result.Error error -> Lwt_result.fail @@ unmap error

  let savepoint_start =
    let open Caqti_request.Infix in
    (Caqti_type.unit -->. Caqti_type.unit) @@:- fun _ -> "SAVEPOINT __sqml;"

  let savepoint_commit =
    let open Caqti_request.Infix in
    (Caqti_type.unit -->. Caqti_type.unit) @@:- fun _ ->
    "RELEASE SAVEPOINT __sqml;"

  let savepoint_rollback =
    let open Caqti_request.Infix in
    (Caqti_type.unit -->. Caqti_type.unit) @@:- fun _ ->
    "ROLLBACK TO SAVEPOINT __sqml;"

  let with_savepoint ~pp_error transaction ?tags f =
    let f ((module C : Caqti_lwt.CONNECTION) as connection) =
      let* () = Logs.debug @@ fun m -> m ?tags "savepoint start" in
      let*! () = C.exec savepoint_start () |> error in
      let f () =
        f
          {
            connection = Lwt_guarded.make connection;
            current_query = transaction.current_query;
            error = None;
          }
        >>= function
        | Result.Error e ->
          let* () =
            Logs.debug @@ fun m -> m ?tags "savepoint rollback: %a" pp_error e
          in
          let*! () = C.exec savepoint_rollback () |> error in
          Lwt_result.fail e
        | Result.Ok res ->
          let* () = Logs.debug @@ fun m -> m ?tags "savepoint commit" in
          let+! () = C.exec savepoint_commit () |> error in
          res
      and catch exn =
        let* () =
          Logs.debug @@ fun m -> m ?tags "savepoint rollback: %a" Fmt.exn exn
        in
        let*! () = C.exec savepoint_rollback () |> error in
        raise exn
      in
      Lwt.catch f catch
    in
    acquire ~f transaction

  let split_pattern pattern in_ =
    let length = String.Search_pattern.pattern pattern |> String.length
    and splits =
      String.Search_pattern.index_all pattern ~may_overlap:false ~in_
    in
    let rest, split =
      List.fold_right splits ~init:(in_, []) ~f:(fun i (s, split) ->
          let pos = i + length in
          ( String.sub s ~pos:0 ~len:i,
            String.sub s ~pos ~len:(String.length s - pos) :: split ))
    in
    rest :: split

  let schema t =
    let comma = String.Search_pattern.create ", "
    and on = String.Search_pattern.create " ON "
    and using = String.Search_pattern.create " USING btree " in
    let schema_of_name = function "public" -> None | name -> Some name in
    let type_of_info ~table:_ _column ~char_length ~nullable ~default:_
        data_type =
      let open Let.Syntax2 (Result) in
      let nullable ((Column.Type.Type t : Column.Type.any_atomic) as type_) =
        if nullable then (Type (Nullable t) : Column.Type.any_atomic) else type_
      in
      let+ type_ =
        match (data_type, char_length) with
        | "bigint", _ ->
          Result.return (Column.Type.Type Bigint : Column.Type.any_atomic)
        | "boolean", _ ->
          Result.return (Column.Type.Type Bool : Column.Type.any_atomic)
        | "character varying", Some l ->
          Result.return (Column.Type.Type (Varchar l) : Column.Type.any_atomic)
        | "character varying", None ->
          Result.return (Column.Type.Type Text : Column.Type.any_atomic)
        | "date", _ ->
          Result.return (Column.Type.Type Date : Column.Type.any_atomic)
        | "integer", _ ->
          Result.return (Column.Type.Type Int : Column.Type.any_atomic)
        | "jsonb", _ ->
          Result.return (Column.Type.Type Json : Column.Type.any_atomic)
        | "text", _ ->
          Result.return (Column.Type.Type Text : Column.Type.any_atomic)
        | ("timestamp" | "timestamp without time zone"), _ ->
          Result.return (Column.Type.Type Time : Column.Type.any_atomic)
        | _ ->
          Result.fail
            (`Sqml_engine
               (Fmt.str "unrecognized data_type in information_schema: %S"
                  data_type))
      in
      nullable type_
    in
    let f (module C : Caqti_lwt.CONNECTION) =
      let open Caqti_request.Infix in
      let*! tables =
        let query =
          (Caqti_type.unit ->* Caqti_type.(t2 string string))
            ~oneshot:true
            {| SELECT schemaname, tablename FROM pg_catalog.pg_tables
               WHERE schemaname != 'pg_catalog'
                 AND schemaname != 'information_schema' |}
        in
        C.collect_list query () |> error
      in
      let f (schema, name) =
        let*! columns =
          let*! columns =
            let query =
              (Caqti_type.(t2 string string)
              ->* Caqti_type.(t4 (option int) string string (t2 string string))
              )
                {| SELECT character_maximum_length,
                        column_name,
                        data_type,
                        column_default,
                        is_nullable
                 FROM information_schema.columns
                 WHERE table_schema = $1
                   AND table_name   = $2 |}
            in
            C.collect_list query (schema, name) |> error
          in
          let f (char_length, column_name, data_type, (default, nullable)) =
            let open Let.Syntax2 (Result) in
            let+ type_ =
              type_of_info ~table:name column_name ~char_length ~default
                ~nullable:String.(nullable = "YES")
                data_type
            in
            ({ Table.Schema.name = column_name; type_ }
              : Table.Schema.column_atomic)
          in
          columns |> List.map ~f |> Result.all |> Lwt.return
        in
        let*! constraints =
          let query =
            (Caqti_type.(t2 string string)
            ->* Caqti_type.(
                  t4 string string string
                    (option
                       (t3 (t2 string string) string
                          (t4 string string string string)))))
              {|
SELECT
    con.conname AS constraint_name,
    src_col.attname AS column_name,
    con.contype AS constraint_type,
    ns_target.nspname AS target_schema,
    cls_target.relname AS target_table,
    tgt_col.attname AS target_column,
    CASE con.confdeltype
        WHEN 'a' THEN 'NO ACTION'
        WHEN 'r' THEN 'RESTRICT'
        WHEN 'c' THEN 'CASCADE'
        WHEN 'n' THEN 'SET NULL'
        WHEN 'd' THEN 'SET DEFAULT'
    END AS delete_rule,
    CASE con.confupdtype
        WHEN 'a' THEN 'NO ACTION'
        WHEN 'r' THEN 'RESTRICT'
        WHEN 'c' THEN 'CASCADE'
        WHEN 'n' THEN 'SET NULL'
        WHEN 'd' THEN 'SET DEFAULT'
    END AS update_rule,
    con.condeferrable AS is_deferrable,
    con.condeferred AS initially_deferred
FROM pg_constraint con
JOIN pg_class cls_src ON cls_src.oid = con.conrelid
JOIN pg_namespace ns_src ON ns_src.oid = cls_src.relnamespace
LEFT JOIN LATERAL (
  SELECT att.attname, att.attnum, k.ord
  FROM unnest(con.conkey) WITH ORDINALITY AS k(attnum, ord)
  JOIN pg_attribute att
    ON att.attrelid = cls_src.oid
   AND att.attnum   = k.attnum
) src_col ON TRUE
LEFT JOIN pg_class cls_target ON cls_target.oid = con.confrelid
LEFT JOIN pg_namespace ns_target ON ns_target.oid = cls_target.relnamespace
LEFT JOIN LATERAL (
  SELECT att.attname, att.attnum, k.ord
  FROM unnest(con.confkey) WITH ORDINALITY AS k(attnum, ord)
  JOIN pg_attribute att
    ON att.attrelid = cls_target.oid
   AND att.attnum   = k.attnum
) tgt_col ON TRUE
WHERE ns_src.nspname = $1 AND cls_src.relname = $2
ORDER BY constraint_name, src_col.ord;|}
          in
          let+! rows = C.collect_list query (schema, name) |> error in
          let f = function
            | (constraint_name, _, "p", _) :: _ as columns ->
              let get_column (_, c, _, _) = c in
              Some
                (Table.Schema.Constraint.Primary_key
                   {
                     name = constraint_name;
                     columns = List.map ~f:get_column columns;
                   })
            | ( constraint_name,
                _,
                "f",
                Some
                  ( (schema, name),
                    _,
                    (delete, update, deferrable, initially_deferred) ) )
              :: _ as columns ->
              let action_of_string s =
                Sqml.Table.referential_action_of_string s
                |> Result.of_option
                     ~error:(Fmt.str "invalid referential action: %S" s)
                |> Result.ok_or_failwith
              and deferrability =
                match (deferrable, initially_deferred) with
                | "f", "f" -> Sqml.Table.Not_deferrable
                | "t", "f" -> Sqml.Table.Deferrable
                | "t", "t" -> Sqml.Table.Deferred_initially
                | _ ->
                  Fmt.failwith
                    "invalid deferrable, initially deffered values: %S, %S"
                    deferrable initially_deferred
              and get_source (_, c, _, _) = c
              and get_target = function
                | _, _, _, Some (_, c, _) -> c
                | _ -> failwith "missing target column in constraint"
              in
              Some
                (Table.Schema.Constraint.Foreign_key
                   {
                     deferrability;
                     name = constraint_name;
                     table = { schema = schema_of_name schema; name };
                     source = List.map ~f:get_source columns;
                     target = List.map ~f:get_target columns;
                     on_delete = action_of_string delete;
                     on_update = action_of_string update;
                   })
            | _ -> None
          in
          rows
          |> List.group ~break:(fun (left, _, _, _) (right, _, _, _) ->
                 String.(left <> right))
          |> List.filter_map ~f
        in
        let*! indexes =
          let query =
            (Caqti_type.(t2 string string) ->* Caqti_type.(t2 string string))
              {| SELECT indexname, indexdef
                 FROM pg_indexes
                 WHERE schemaname = $1 AND tablename = $2 |}
          in
          let*! rows = C.collect_list query (schema, name) |> error in
          let f (name, def) =
            let open Let.Syntax2 (Result) in
            let fail =
              Result.fail
                (`Sqml_engine
                   (Fmt.str "unrecognized index definition for %s: %s" name def))
            in
            let* def, unique =
              match
                ( String.chop_prefix def ~prefix:"CREATE INDEX ",
                  String.chop_prefix def ~prefix:"CREATE UNIQUE INDEX " )
              with
              | Some def, None -> Result.Ok (def, false)
              | None, Some def -> Result.Ok (def, true)
              | _ -> fail
            in
            match split_pattern on def with
            | [ name; def ] -> (
              match split_pattern using def with
              | [ _; def ] ->
                let def = String.sub def ~pos:1 ~len:(String.length def - 2) in
                let* columns =
                  match
                    split_pattern comma def
                    |> List.map ~f:(fun c ->
                           ( c,
                             Sqml.Table.Schema.Index.
                               { ascending = true; nulls_first = None }
                             (* FIXME *) ))
                  with
                  | head :: tail -> Result.Ok List_nonempty.(head :: tail)
                  | _ -> fail
                in
                Result.Ok
                  {
                    Sqml.Table.Schema.Index.columns;
                    name = { name; schema = schema_of_name schema };
                    unique;
                  }
              | _ -> fail)
            | _ -> fail
          in
          List.map ~f rows
          |> List.filter_map ~f:(function
               | Result.Ok res -> Some res
               | Result.Error (`Sqml_engine error) ->
                 let () =
                   Logs.Sync.warn (fun m -> m "ignoring index: %s" error)
                 in
                 None)
          |> Lwt_result.return
        in
        Lwt_result.return
          {
            constraints;
            Sqml.Table.Schema.columns =
              Set.of_list (module Table.Schema.Column_by_name) columns;
            indexes;
            name = { name; schema = schema_of_name schema };
          }
      in
      Sequence.of_list tables |> Sequence.Seq.map ~f |> Sequence.to_list
    in
    connect t (fun transaction -> Lwt_guarded.acquire ~f transaction.connection)
end
