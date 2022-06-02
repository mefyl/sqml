include module type of Model_intf

module Make (Meta : sig
  val version : Semver.t
end) : S

val create : (module S with type tag = 'tag) -> Table.Query.t list

val diff :
  Dialect.t ->
  (module S with type tag = 'tag) ->
  Table.Schema.t list ->
  (Table.Query.t list, string) Utils.Either.t list
