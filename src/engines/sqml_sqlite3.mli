module Make (Model : Sqml.Model.S) : sig
  include Sqml.Engine.S with module Model = Model

  val make :
    ?uri:string ->
    unit ->
    (Model.tag t, [> `Sqml_engine of string ]) Lwt_result.t

  val of_db :
    Sqlite3.db -> (Model.tag t, [> `Sqml_engine of string ]) Lwt_result.t
end
