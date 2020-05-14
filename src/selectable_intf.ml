open Base

(** Any object that can be selected from *)

module type S = sig
  (** Type identifier of columns that can be selected from this *)
  type tag

  val pp : Formatter.t -> unit -> unit

  val pp_ : prefix:string -> Formatter.t -> unit -> unit
end

type 'tag t = (module S with type tag = 'tag)
