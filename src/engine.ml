open Acid

module type S = sig
  type 'model t

  module Model : Model.S

  val query :
    Model.tag t ->
    ('t, 'arity) Model.Query.t ->
    ( ('t, 'arity, ([> `Sqml_engine of string ] as 'error)) Model.Query.result,
      'error )
    Lwt_result.t
end
