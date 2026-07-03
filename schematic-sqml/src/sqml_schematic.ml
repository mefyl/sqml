open Acid

module Enum = struct
  type 't case = {
    name : string;
    encoder : 't -> string option;
    decoder : string -> 't;
  }

  (** The type for an enum, i.e. a union where all the fields are nullary
      constructors.

      These are important for SQML because they can be represented as a single
      [Text] column in the database. *)
  type 't t = 't case list

  let of_generic_case : type a. a Schematic.Schema.case -> a case option =
   fun (Schematic.Schema.Case { name; encoder; decoder; schema; _ }) ->
    match schema.descriptor with
    | String_const const_string ->
      Some
        {
          name;
          encoder =
            (fun value ->
              if Option.is_some (encoder value) then Some const_string else None);
          decoder = (fun _text -> decoder ());
        }
    | _ -> None

  let of_generic_cases cases = Option.all @@ List.map ~f:of_generic_case cases
end

let rec atomic_type_of_descriptor : type t.
    ?id:string ->
    t Schematic.Schema.descriptor ->
    t Sqml.Expression.Column.Type.atomic =
 fun ?id descriptor ->
  let fail name =
    Fmt.failwith "sqml: %acannot bind %s descriptor@ @[%a@]"
      Fmt.(option (const string "schema " ++ string ++ const char ':' ++ sp))
      id name Schematic.Schema.pp_descriptor descriptor
  in
  match descriptor with
  | Any _ -> fail "Any"
  | Bool -> Bool
  | Coding { f } -> atomic_type_of_descriptor ?id (f None)
  | Float -> Float
  | Date ->
    let decode = Fn.compose Result.return Timmy.Date.to_tuple
    and encode = Timmy.Date.of_tuple_exn ~here:[%here] in
    Custom { decode; encode; repr = Date }
  | Fixpoint s -> atomic_type_of_descriptor ?id (Lazy.force s)
  | Int -> Int
  | Ignore -> fail "Ignore"
  | Int64 -> Bigint
  | Json -> Json
  | List _ -> fail "List"
  | Sequence _ -> fail "Sequence"
  | Null -> fail "Null"
  | String -> Text
  | Time ->
    Custom
      {
        repr = Time;
        encode = Timmy.Time.of_ptime;
        decode = Fn.compose Result.return Timmy.Time.to_ptime;
      }
  | Time_local -> fail "Time_local (FIXME)"
  | Map { decode; encode; descriptor } ->
    Custom { decode; encode; repr = atomic_type_of_descriptor descriptor }
  | Mapping _ -> fail "Mapping"
  | Nullable schema -> Nullable (atomic_type_of_descriptor schema.descriptor)
  | String_const _ -> fail "String_const"
  | Object _ -> fail "Object"
  | Tuple _ -> fail "Tuple"
  | Union { cases; _ } -> (
    match Enum.of_generic_cases cases with
    | Some cases ->
      let encode value =
        let rec find_case = function
          | [] -> Fmt.failwith "sqml: enum value not found in any case"
          | { Enum.encoder; name; _ } :: rest -> (
            match encoder value with Some _ -> name | None -> find_case rest)
        in
        find_case cases
      in
      let decode text =
        let rec find_case = function
          | [] -> Error (Fmt.str "sqml: invalid enum value %S" text)
          | { Enum.decoder; name; _ } :: rest ->
            if String.equal name text then Ok (decoder name) else find_case rest
        in
        find_case cases
      in
      Custom { decode; encode; repr = Text }
    | None -> fail "Union")
  | Well_known { descriptor; _ } -> atomic_type_of_descriptor ?id descriptor

and atomic_type_of_schema Schematic.Schema.{ descriptor; id; _ } =
  atomic_type_of_descriptor ?id descriptor

let rec types_of_fields : type l.
    l Hlist.t Schematic.Schema.fields -> l Sqml.Expression.Column.Type.List.t =
  function
  | Field { field = { name; requirement; schema; _ }; rest } -> (
    let rest = types_of_fields rest in
    match requirement with
    | Default _ ->
      (name, atomic_type_of_schema (Schematic.Schema.field_schema schema))
      :: rest
    | Optional ->
      ( name,
        Nullable (atomic_type_of_schema (Schematic.Schema.field_schema schema))
      )
      :: rest
    | Required ->
      (name, atomic_type_of_schema (Schematic.Schema.field_schema schema))
      :: rest
    | Computed | Computed_optional ->
      Fmt.failwith "sqml: cannot bind computed field %S" name)
  | FieldEnd -> []

let rec hlist_of_fields : type l.
    l Hlist.t Schematic.Schema.fields -> l Hlist.t -> l Hlist.t = function
  | Field { field = _; rest } ->
    let rest = hlist_of_fields rest in
    fun (hd :: tl) -> hd :: rest tl
  | FieldEnd -> fun [] -> []

let rec type_of_descriptor : type t.
    ?id:string ->
    t Schematic.Schema.descriptor ->
    t Sqml.Expression.Column.Type.t =
 fun (type t) ?id (descriptor : t Schematic.Schema.descriptor) :
     t Sqml.Expression.Column.Type.t ->
  match descriptor with
  | Any _ | Bool | Float | Date | Int | Ignore | Int64 | Json | List _
  | Mapping _ | Null | Sequence _ | String | Time | Time_local | String_const _
  | Tuple _ | Union _ ->
    Atomic (atomic_type_of_descriptor descriptor)
  | Coding { f } -> type_of_descriptor ?id (f None)
  | Fixpoint s -> type_of_descriptor ?id (Lazy.force s)
  | Map { encode; decode; descriptor; _ } ->
    Custom { encode; decode; repr = type_of_descriptor descriptor }
  | Nullable t -> Nullable (type_of_descriptor ?id t.descriptor)
  | Object { decode; encode; fields } ->
    let columns = types_of_fields fields
    and hlist_of_fields = hlist_of_fields fields in
    Composite
      {
        columns;
        decode = (fun columns -> decode columns);
        encode = (fun o -> encode o |> hlist_of_fields);
      }
  | Well_known { descriptor; _ } -> type_of_descriptor ?id descriptor

and type_of_schema Schematic.Schema.{ descriptor; id; _ } =
  type_of_descriptor ?id descriptor
