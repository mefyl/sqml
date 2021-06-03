include module type of Model_intf

module Make (Meta : sig
  val version : Semver.t
end) : S

val schema : Dialect.t -> t -> string list
