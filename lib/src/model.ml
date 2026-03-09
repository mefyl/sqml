open Utils
include Model_intf

module Make (Meta : sig
  val version : Semver.t
end) =
struct
  let version = Meta.version

  type tag = unit
  type t = { mutable tables : (module Table.S with type tag = tag) Array.t }

  let v = { tables = [||] }
  let tables () = v.tables

  module BindTable (Meta : Table.META) = struct
    module Res = struct
      type model = unit

      module Meta = Meta

      type tag = unit

      let name = Meta.name
      let selected : tag Selected.t = Selected.Private.v { name }

      module Column = struct
        type nonrec 't t = ('t, tag) Expression.Column.t

        let rec bind_columns : type l.
            l Table.Column.List.t ->
            (l, tag) Expression.Column.List.t * tag Table.constraint_ list =
          function
          | Table.Column.List.({ foreign_keys; name; type_; _ } :: tl) ->
            let column =
              Expression.make_column ~name ~selectable:Meta.name type_
            in
            let constraints =
              List.map
                ~f:(function
                  | Table.Column.Foreign_key.Reference
                      {
                        deferrability;
                        reference = target;
                        on_delete;
                        on_update;
                      } ->
                    Table.(
                      Foreign_key
                        {
                          deferrability;
                          reference =
                            List_nonempty.
                              [ Foreign_reference { column; target } ];
                          on_delete;
                          on_update;
                        })
                  | Table.Column.Foreign_key.Reference_opt
                      {
                        deferrability;
                        reference = target;
                        on_delete;
                        on_update;
                      } ->
                    Table.(
                      Foreign_key
                        {
                          deferrability;
                          reference =
                            List_nonempty.
                              [ Foreign_reference_opt { column; target } ];
                          on_delete;
                          on_update;
                        }))
                foreign_keys
            and rest, rest_constraints = bind_columns tl in
            ( Expression.Column.List.(column :: rest),
              constraints @ rest_constraints )
          | [] -> (Expression.Column.List.[], [])

        let columns, constraints = bind_columns Meta.columns
      end

      let column_types =
        let rec type_of_columns : type a.
            (a, _) Expression.Column.List.t -> a Expression.Type.List.t =
          function
          | hd :: tl ->
            Expression.Type.of_column_type hd.type_ :: type_of_columns tl
          | [] -> []
        in
        type_of_columns Column.columns

      let type_values =
        let rec type_of_columns : type a.
            (a, _) Expression.Column.List.t -> a Hlist.t Expression.Type.t =
          function
          | hd :: tl ->
            Expression.Type.of_column_type hd.type_ :: type_of_columns tl
          | [] -> []
        in
        type_of_columns Column.columns

      let type_ =
        let decode columns = Meta.map columns |> Result.return
        and encode = Meta.unmap in
        Expression.Type.Custom { decode; encode; repr = type_values }

      let const value = Expression.const type_ value

      module Expression = struct
        let rec bind_expressions : type l tag.
            (l, tag) Expression.Column.List.t -> (l, tag) Expression.List.t =
          function
          | Expression.Column.List.(head :: tail) ->
            Expression.column head :: bind_expressions tail
          | Expression.Column.List.[] -> []

        let columns = bind_expressions Column.columns

        include Expression

        type nonrec 't t = ('t, tag) t
        type nonrec 't column = ('t, tag) Column.t

        module List = struct
          include List

          type 't t = ('t, tag) List.t
          type 't value = ('t, tag) List.value
        end
      end

      let all =
        let columns = Expression.(of_list columns) in
        Expression.map
          ~decode:(fun v -> Result.return (Meta.map v))
          ~encode:Meta.unmap columns

      let make columns =
        let columns = Expression.of_list columns in
        Expression.map
          ~decode:(fun v -> Result.return (Meta.map v))
          ~encode:Meta.unmap columns

      let to_ast () = Expression.Selectable { alias = None; name }
      let constraints = Column.constraints @ Meta.constraints Column.columns

      module Index_of_columns =
        Expression.Column.List.Transduce2 (Table.Index.Expression.List)

      let primary =
        let expressions =
          Index_of_columns.map
            (Meta.primary Column.columns)
            ~f:
              {
                f =
                  (fun column ->
                    {
                      Table.Index.Expression.expression =
                        Expression.column column;
                      order = Ascending;
                    });
              }
        in
        Table.Index.Unique { expressions; name = "pkey" }

      let indexes = Meta.indexes Column.columns

      let values (values : Meta.columns Expression.List.t) :
          Meta.mapped Expression.t =
        Expression.Private.conceal (type_, Expression.(to_ast @@ of_list values))
    end

    let _ = v.tables <- Array.append v.tables [| (module Res) |]

    include Res
  end

  module type JOIN = sig
    include Selectable.S
    module L : Selectable.S with type model = model
    module R : Selectable.S with type model = model

    val selected_left : L.tag Selected.t -> tag Selected.t
    val selected_right : R.tag Selected.t -> tag Selected.t
  end

  module type INNER_JOIN = sig
    include JOIN

    val left : ('t, L.tag) Expression.t -> ('t, tag) Expression.t
    val right : ('t, R.tag) Expression.t -> ('t, tag) Expression.t
  end

  module type LEFT_OUTER_JOIN = sig
    include JOIN

    val left : ('t, L.tag) Expression.t -> ('t, tag) Expression.t
    val right : ('t, R.tag) Expression.t -> ('t Option.t, tag) Expression.t
  end

  module type RIGHT_OUTER_JOIN = sig
    include JOIN

    val left : ('t, L.tag) Expression.t -> ('t Option.t, tag) Expression.t
    val right : ('t, R.tag) Expression.t -> ('t, tag) Expression.t
  end

  module type OUTER_JOIN = sig
    include JOIN

    val left : ('t, L.tag) Expression.t -> ('t Option.t, tag) Expression.t
    val right : ('t, R.tag) Expression.t -> ('t Option.t, tag) Expression.t
  end

  module State_monad = struct
    module Base = struct
      type ('a, 'state) t = 'state -> 'a * 'state

      let bind (v : ('a, 'state) t) ~(f : 'a -> ('b, 'state) t) : ('b, 'state) t
          =
       fun state ->
        let v, state = v state in
        f v state

      let map = `Define_using_bind
      let return v state = (v, state)
    end

    include Base
    include Monad.Make2 (Base)
  end

  module Traverse = Expression.Traverse_ast (State_monad)

  let map_name ast ~f =
    let f ast mapped =
      match ast with
      | Expression.Column ({ selectable; _ } as pl)
        when Set.mem mapped selectable ->
        (Expression.Column { pl with selectable = f selectable }, mapped)
      | Selectable { name; alias } ->
        let alias = Option.value ~default:name alias in
        (Selectable { name; alias = Some (f alias) }, Set.add mapped alias)
      | ast -> (ast, mapped)
    in
    Traverse.traverse_ast ast ~f (Set.empty (module String)) |> fst

  module Join
      (L : Selectable.S with type model = tag)
      (R : Selectable.S with type model = tag) =
  struct
    module Content = struct
      module type INNER =
        INNER_JOIN with type model = tag and module L = L and module R = R

      module type OUTER =
        OUTER_JOIN with type model = tag and module L = L and module R = R

      module type LEFT_OUTER =
        LEFT_OUTER_JOIN with type model = tag and module L = L and module R = R

      module type RIGHT_OUTER =
        RIGHT_OUTER_JOIN with type model = tag and module L = L and module R = R

      type model = tag
      type tag = unit

      module L = L
      module R = R

      let selected_left =
       fun selected ->
        Selected.Private.v { name = "l_" ^ Selected.name selected }

      let selected_right =
       fun selected ->
        Selected.Private.v { name = "r_" ^ Selected.name selected }

      let left : type a. (a, L.tag) Expression.t -> (a, tag) Expression.t =
       fun e -> Expression.map_name (fun n -> "l_" ^ n) e

      let right : type a. (a, R.tag) Expression.t -> (a, tag) Expression.t =
       fun e -> Expression.map_name (fun n -> "r_" ^ n) e

      let to_ast_ type_ on =
        Expression.Join
          {
            left = map_name ~f:(fun n -> "l_" ^ n) @@ L.to_ast ();
            right = map_name ~f:(fun n -> "r_" ^ n) @@ R.to_ast ();
            on;
            type_;
          }
    end

    let on expr =
      (module struct
        include Content

        let to_ast () = to_ast_ Inner (Some (Expression.to_ast expr))
      end : Content.INNER)

    let outer_on expr =
      (module struct
        include Content

        let left e =
          let e = left e in
          Expression.some e

        let right e =
          let e = right e in
          Expression.some e

        let to_ast () = to_ast_ Outer (Some (Expression.to_ast expr))
      end : Content.OUTER)

    let left_on expr =
      (module struct
        include Content

        let right e =
          let e = right e in
          Expression.some e

        let to_ast () = to_ast_ Outer_left (Some (Expression.to_ast expr))
      end : Content.LEFT_OUTER)

    let right_on expr =
      (module struct
        include Content

        let left e =
          let e = left e in
          Expression.some e

        let to_ast () = to_ast_ Outer_right (Some (Expression.to_ast expr))
      end : Content.RIGHT_OUTER)

    include Content

    let to_ast () = to_ast_ Inner None
  end

  module Query = struct
    type ('t, 'tag) delete = {
      from : (module Table.S with type model = tag and type tag = 'tag);
      where : (bool, 'tag) Expression.t option;
      returning : ('t, 'tag) Expression.t;
    }

    type 't select =
      | Select : {
          for' : Expression.for' option;
          from :
            (module Selectable.S with type model = tag and type tag = 'tag);
          group_by : (_, 'tag) Expression.t option;
          limit : int option;
          order_by : 'tag Order_by.t;
          skip_locked : bool;
          what : ('t, _) Expression.t;
          where : (bool, 'tag) Expression.t option;
        }
          -> 't select

    type ('t, 'tag, 'from) update = {
      into : (module Table.S with type model = tag and type tag = 'tag);
      from :
        (module Selectable.S with type model = tag and type tag = 'from) option;
      what : 'tag Expression.assignment list;
      where : (bool, 'tag) Expression.t option;
      returning : ('t, 'tag) Expression.t;
    }

    type ('t, 'arity) t =
      | Delete : {
          from : (module Table.S with type model = tag and type tag = 'tag);
          where : (bool, 'tag) Expression.t option;
        }
          -> (unit, zero) t
      | Delete_returning : ('t, 'tag) delete -> ('t, many) t
      | Delete_unique : ('t, 'tag) delete -> ('t, one_maybe) t
      | Insert : {
          excluded : 't. ('t, 'tag) Expression.t -> ('t, 'tag) Expression.t;
          into :
            (module Table.S
               with type model = tag
                and type Meta.mapped = 'mapped
                and type tag = 'tag);
          on_conflict : (_, 'tag) on_conflict;
          on_conflict_index : (_ Table.Index.unique, 'tag) Table.Index.t option;
          value : ('mapped, 'free) Expression.t;
        }
          -> (unit, zero) t
      | Insert_returning : {
          excluded : 't. ('t, 'tag) Expression.t -> ('t, 'tag) Expression.t;
          into :
            (module Table.S
               with type model = tag
                and type Meta.mapped = 'mapped
                and type tag = 'tag);
          on_conflict : ('arity, 'tag) on_conflict;
          on_conflict_index : (_ Table.Index.unique, 'tag) Table.Index.t option;
          returning : ('t, 'tag) Expression.t;
          value : ('mapped, 'free) Expression.t;
        }
          -> ('t, 'arity) t
      | Select : 't select -> ('t, many) t
      | Select_unique : 't select -> ('t, one_maybe) t
      | Update : {
          into : (module Table.S with type model = tag and type tag = 'tag);
          what : 'tag Expression.assignment list;
          where : (bool, 'tag) Expression.t option;
        }
          -> (unit, zero) t
      | Update_returning : ('t, 'tag, 'from) update -> ('t, many) t
      | Update_unique : ('t, 'tag, 'from) update -> ('t, one_maybe) t

    let arity : type a tag. (tag, a) t -> a arity = function
      | Delete _ -> Zero
      | Delete_returning _ -> Many
      | Delete_unique _ -> One_maybe
      | Insert _ -> Zero
      | Insert_returning { on_conflict = Fail; _ } -> One
      | Insert_returning { on_conflict = Set _; _ } -> One
      | Insert_returning { on_conflict = Update; _ } -> One
      | Insert_returning { on_conflict = Ignore; _ } -> One_maybe
      | Select _ -> Many
      | Select_unique _ -> One_maybe
      | Update _ -> Zero
      | Update_returning _ -> Many
      | Update_unique _ -> One_maybe
  end

  let group (type group selectable)
      ~(from :
         (module Selectable.S with type model = tag and type tag = selectable))
      ~(by : (group, selectable) Expression.Column.t) ?limit
      ?(order_by = Order_by.[]) ?where what =
    let column = Expression.column by in
    let what =
      what (column |> Expression.Private.reify |> Expression.Private.conceal)
    in
    Query.Select
      (Select
         {
           for' = None;
           from;
           group_by = Some column;
           limit;
           order_by;
           skip_locked = false;
           what;
           where;
         })

  let for_to_ast for' =
    match for' with
    | Some Expression.For.Key_share -> Some Expression.Key_share
    | Some Expression.For.No_key_update -> Some Expression.No_key_update
    | Some Share -> Some Share
    | Some Update -> Some Update
    | Some (Update_of tables) ->
      Some (Update_of (List.map ~f:Selected.name tables))
    | None -> None

  let select ~from ?for' ?limit ?(order_by = Order_by.[]) ?(skip_locked = false)
      ?where what =
    Query.Select
      (Select
         {
           for' = for_to_ast for';
           from;
           group_by = None;
           limit;
           order_by;
           skip_locked;
           what;
           where;
         })

  module Indexed = Hlist.Zip2 (Table.Index.Expression.List) (Expression.List)

  let unique (type columns tag)
      (index : (columns, tag) Table.Index.Expression.List.t)
      (indexed : (columns, tag) Expression.List.t)
      (where : (bool, tag) Expression.t option) :
      (bool, tag) Expression.t option =
    let open Expression in
    let indexed =
      Indexed.zip index indexed
      |> Indexed.map
           ~f:
             {
               f =
                 (fun ({ Table.Index.Expression.expression; _ }, value) ->
                   expression = value);
             }
    in
    match indexed with
    | [] -> where
    | hd :: tl -> (
      let filter = Base.List.fold ~init:hd ~f:(fun l r -> l && r) tl in
      match where with
      | None -> Some filter
      | Some where -> Some (filter && where))

  let select_unique ~from (Table.Index.Unique { expressions = index; _ }) indexed
      ?for' ?(skip_locked = false) ?where what =
    let where = unique index indexed where in
    Query.Select_unique
      (Select
         {
           for' = for_to_ast for';
           from;
           group_by = None;
           limit = None;
           order_by = [];
           skip_locked;
           what;
           where;
         })

  let select_one ~from ?for' ?(order_by = Order_by.[]) ?(skip_locked = false)
      ?where what =
    Query.Select_unique
      (Select
         {
           for' = for_to_ast for';
           from;
           group_by = None;
           limit = Some 1;
           order_by;
           skip_locked;
           what;
           where;
         })

  let insert ~into ?on_conflict ?on_conflict_index value =
    match on_conflict with
    | None ->
      Query.Insert
        {
          excluded = (fun e -> Expression.map_name (fun _ -> "excluded") e);
          into;
          on_conflict = Fail;
          on_conflict_index;
          value;
        }
    | Some on_conflict ->
      Query.Insert
        {
          excluded = (fun e -> Expression.map_name (fun _ -> "excluded") e);
          into;
          on_conflict;
          on_conflict_index;
          value;
        }

  let insert_returning ~into ~on_conflict ?on_conflict_index ~returning value =
    Query.Insert_returning
      {
        excluded = (fun e -> Expression.map_name (fun _ -> "excluded") e);
        into;
        on_conflict;
        on_conflict_index;
        returning;
        value;
      }

  let delete ~from ?where () = Query.Delete { from; where }

  let delete_returning ~from ?where returning =
    Query.Delete_returning { from; returning; where }

  let delete_unique ~from (Table.Index.Unique { expressions = index; _ }) indexed
      ?where returning =
    let where = unique index indexed where in
    Query.Delete_unique { from; returning; where }

  let update into ?where what = Query.Update { into; where; what }

  let update_returning into ?where ~returning what =
    Query.Update_returning { from = None; into; returning; what; where }

  let update_unique into (Table.Index.Unique { expressions = index; _ }) indexed
      ?where ~returning what =
    let where = unique index indexed where in
    Query.Update_unique { from = None; into; returning; what; where }

  let update_from_returning (type from) into
      ~from:
        (module From : Selectable.S with type model = tag and type tag = from)
      ?where ~returning what =
    let rename name = "from_" ^ name in
    let map e = Expression.map_name rename e in
    let from =
      (module struct
        include From

        let to_ast () = map_name ~f:rename (to_ast ())
      end : Selectable.S
        with type model = tag
         and type tag = from)
    and returning = returning { map }
    and what = what { map }
    and where = Option.map ~f:(fun where -> where { map }) where in
    Query.Update_returning { from = Some from; into; returning; what; where }

  let update_from_unique (type from) into
      (Table.Index.Unique { expressions = index; _ }) indexed
      ~from:
        (module From : Selectable.S with type model = tag and type tag = from)
      ?where ~returning what =
    let rename name = "from_" ^ name in
    let map e = Expression.map_name rename e in
    let from =
      (module struct
        include From

        let to_ast () = map_name ~f:rename (to_ast ())
      end : Selectable.S
        with type model = tag
         and type tag = from)
    and returning = returning { map }
    and what = what { map }
    and where =
      unique index indexed (Option.map where ~f:(fun where -> where { map }))
    in
    Query.Update_unique { from = Some from; into; returning; what; where }

  (* Metadata *)

  module Metadata = struct
    type t = {
      major : int;
      minor : int;
      subminor : int;
      migrated : Timmy.Time.t;
    }

    module Meta_ = struct
      type mapped = t

      let schema = Some "sqml"
      let name = "metadata"
      let major = Table.Column.(make "major" Type.Int)
      let minor = Table.Column.(make "minor" Type.Int)
      let subminor = Table.Column.(make "subminor" Type.Int)
      let migrated = Table.Column.(make "migrated" Type.Time)

      type columns = int -> int -> int -> Timmy.Time.t -> unit

      let columns = Table.Column.List.[ major; minor; subminor; migrated ]

      let map Hlist.[ major; minor; subminor; migrated ] =
        { major; minor; subminor; migrated }

      let unmap { major; minor; subminor; migrated } =
        Hlist.[ major; minor; subminor; migrated ]

      let constraints _ = []

      type primary = int -> int -> int -> unit

      let primary Expression.Column.List.[ major; minor; subminor; _ ] =
        Expression.Column.List.[ major; minor; subminor ]

      type indexes = unit

      let indexes _ = Table.Index.List.[]
    end

    include BindTable (Meta_)
  end
end

let create (type tag) (module Model : S with type tag = tag) =
  Array.map ~f:Table.Query.create (Model.tables ())
  |> Array.to_list
  |> List.concat

let diff (type tag) (module Dialect : Dialect.S)
    (module Model : S with type tag = tag) meta =
  let module Seq = Base.Sequence in
  let effective =
    List.map meta ~f:(fun (t : Table.Schema.t) ->
        (Dialect.table_name ?schema:t.name.schema t.name.name, t))
    |> Map.of_alist_exn (module String)
  and expected =
    Model.tables ()
    |> Array.to_list
    |> List.map ~f:(fun ((module T : Table.S with type tag = tag) as t) ->
           ( Dialect.table_name ?schema:T.Meta.schema T.Meta.name,
             Table.Schema.of_table t ))
    |> Map.of_alist_exn (module String)
  and f = function
    | _, `Left t -> [ Table.Query.Schema.drop t ]
    | _, `Right t -> [ Table.Query.Schema.create t ]
    | _, `Unequal (effective, expected) ->
      let columns_diff =
        let effective_columns, expected_columns =
          let map columns =
            let f (({ name; _ } : Table.Schema.column_atomic) as c) =
              (name, c)
            in
            columns
            |> Set.to_list
            |> List.map ~f
            |> Map.of_alist_exn (module String)
          in
          (map effective.Table.Schema.columns, map expected.Table.Schema.columns)
        in
        let diff =
          Map.symmetric_diff ~data_equal:Table.Schema.equal_column_atomic
            effective_columns expected_columns
        in
        Seq.map diff ~f:(function
          | _, `Left c -> Table.Query.Schema.drop_column effective.name c
          | _, `Right c -> [ Table.Query.Schema.add_column effective.name c ]
          | ( _,
              `Unequal
                ( Table.Schema.{ name; type_ = l; _ },
                  Table.Schema.{ type_ = r; _ } ) ) ->
            let rec f = function
              | ( (Type (Nullable l) : Expression.Column.Type.any_atomic),
                  (Type (Nullable r) : Expression.Column.Type.any_atomic) ) ->
                f (Type l, Type r)
              | ( Expression.Column.Type.Type (Nullable l),
                  Expression.Column.Type.Type r ) ->
                Table.Query.Schema.alter_column_nullable effective.name name
                  false
                :: f (Type l, Type r)
              | ( Expression.Column.Type.Type l,
                  Expression.Column.Type.Type (Nullable r) ) ->
                Table.Query.Schema.alter_column_nullable effective.name name
                  true
                :: f (Type l, Type r)
              | l, r when Expression.Column.Type.equal_any_atomic l r -> []
              | Type Bigint, Type Serial_big | Type Int, Type Serial ->
                (* SERIAL is actually INT with a sequence default *)
                []
              | _ ->
                [ Table.Query.Schema.alter_column_type effective.name name l r ]
            in
            f (l, r))
      and constraints_diff =
        let effective_constraints, expected_constraints =
          let set = Set.of_list (module Table.Schema.Constraint) in
          ( set effective.Table.Schema.constraints,
            set expected.Table.Schema.constraints )
        and f = function
          | Either.First (Table.Schema.Constraint.Foreign_key { name; _ })
          | Either.First (Table.Schema.Constraint.Primary_key { name; _ }) ->
            Table.Query.Schema.drop_constraint effective.name name
          | Either.Second c ->
            [ Table.Query.Schema.add_constraint effective.name c ]
        in
        Set.symmetric_diff effective_constraints expected_constraints
        |> Seq.map ~f
      and indexes_diff =
        let obsolete_indexes =
          List.concat_map effective.Table.Schema.indexes ~f:(fun index ->
              let name = index.Table.Schema.Index.name in
              match
                List.find
                  ~f:(Table.Schema.Index.equal index)
                  expected.Table.Schema.indexes
              with
              | Some _ -> []
              | None -> (
                match
                  List.find expected.constraints ~f:(function
                    | Primary_key { name = constraint_name; _ } ->
                      String.(constraint_name = name.name)
                    | _ -> false)
                with
                | None -> Table.Query.Schema.drop_index name
                | Some _ ->
                  (* Primary keys underlying indexes are handled by adding the
                     constraint. *)
                  []))
        and missing_indexes =
          List.concat_map expected.Table.Schema.indexes ~f:(fun index ->
              match
                List.find
                  ~f:(Table.Schema.Index.equal index)
                  effective.Table.Schema.indexes
              with
              | Some _ -> []
              | None -> Table.Query.Schema.create_index effective.name index)
        in
        Seq.of_list [ obsolete_indexes; missing_indexes ]
      in
      Seq.of_list [ columns_diff; constraints_diff; indexes_diff ]
      |> Seq.concat
      |> Seq.to_list
  in
  Map.symmetric_diff ~data_equal:Table.Schema.equal effective expected
  |> Seq.map ~f
  |> Seq.to_list
  |> List.concat
  |> List.concat
