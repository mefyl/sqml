open Acid

type error = [ `Sqml_engine of string ]

module type S = sig
  type 'model t

  module Model : Model.S

  val query :
    Model.tag t ->
    ('t, 'arity) Model.Query.t ->
    ( ('t, 'arity, ([> error ] as 'error)) Model.Query.result,
      'error )
    Lwt_result.t

  val prepare :
    'arg Expression.type_ ->
    (('arg, 'tag) Expression.t -> ('res, 'arity) Model.Query.t) ->
    Model.tag t ->
    'arg ->
    ( ('res, 'arity, ([> error ] as 'error)) Model.Query.result,
      'error )
    Lwt_result.t
end
