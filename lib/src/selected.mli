type 'tag t

val name : _ t -> string

module Private : sig
  val v : 'tag Selected_private.t -> 'tag t
end
