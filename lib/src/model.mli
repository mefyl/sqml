include module type of Model_intf

module Make (Meta : sig
  val version : Semver.t
end) : S

val create : 'tag. (module S with type tag = 'tag) -> Table.Query.t list

val diff :
  'tag.
  Dialect.t ->
  (module S with type tag = 'tag) ->
  Table.Schema.t list ->
  (Table.Query.t list, string) Utils.Either.t list
