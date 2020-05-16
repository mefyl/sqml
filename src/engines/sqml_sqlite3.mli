module Make (Model : Sqml.Model.S) : sig
  include Sqml.Engine.S with module Model = Model

  val make : unit -> (Model.tag t, [> `Sqml_engine of string ]) Lwt_result.t
end
