include module type of Model_intf

module Make (Meta : sig
  val version : Semver.t
end) : S

val create : t -> Table.Query.t list

val diff :
  Dialect.t ->
  (module S) ->
  Table.Schema.t list ->
  [> `Alter of
     Table.Schema.t
     * [> `Create of Table.Schema.column | `Drop of Table.Schema.column ] list
  | `Create of Table.Schema.t
  | `Drop of Table.Schema.t
  ]
  List.t
