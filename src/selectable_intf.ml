(** Any object that can be selected from *)

module type S = sig
  type model

  (** Type identifier of columns that can be selected from this *)
  type tag

  val pp_selectable : unit Fmt.t

  val pp_selectable_ : prefix:string -> unit Fmt.t
end

type 'tag t = (module S with type tag = 'tag)
