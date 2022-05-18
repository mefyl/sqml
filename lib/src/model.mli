include module type of Model_intf

module Make (Meta : sig
  val version : Semver.t
end) : S

val table_schema : Dialect.t -> _ Table.t -> string list
val schema : Dialect.t -> t -> string list

val diff :
  Table.Schema.t list ->
  (module S) ->
  [> `Alter of
     Table.Schema.t
     * [> `Create of Table.Schema.column | `Drop of Table.Schema.column ] list
  | `Create of Table.Schema.t
  | `Drop of Table.Schema.t
  ]
  List.t
