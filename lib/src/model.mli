include module type of Model_intf

module Make (_ : sig
  val version : Semver.t
end) : S

(** [create model] are the statements creating [model] from scratch. *)
val create : (module S with type tag = 'tag) -> Table.Query.t list

(** [pp_create dialect] prints the SQL script creating a model from scratch, as
    run by engines on a fresh database. *)
val pp_create : Dialect.t -> (module S with type tag = 'tag) Fmt.t

val diff :
  Dialect.t ->
  (module S with type tag = 'tag) ->
  Table.Schema.t list ->
  Table.Query.t list
