include module type of Model_intf

module Make (Meta : sig
  val version : Semver.t
end) : S

val table_schema : Dialect.t -> _ Table.t -> string list

val schema : Dialect.t -> t -> string list
