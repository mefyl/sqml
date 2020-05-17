open Acid
module Sequence = Sequence.Make2 (Lwt_result)

module type S = sig
  type 'model t

  module Model : Model.S

  val insert :
    Model.tag t ->
    (module Table.S with type Meta.columns = 'columns) ->
    'columns ->
    (unit, [> `Sqml_engine of string ]) Lwt_result.t

  val query :
    Model.tag t ->
    't Model.Query.t ->
    ( ('t, ([> `Sqml_engine of string ] as 'error)) Sequence.t,
      'error )
    Lwt_result.t
end
