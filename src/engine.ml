open Acid

type error = [ `Sqml_engine of string ]

module type S = sig
  type t

  type transaction

  module Model : Model.S

  val query :
    transaction ->
    ('t, 'arity) Model.Query.t ->
    ( ('t, 'arity, ([> error ] as 'error)) Model.Query.result,
      'error )
    Lwt_result.t

  val prepare :
    'arg Expression.Type.t ->
    (('arg, 'tag) Expression.t -> ('res, 'arity) Model.Query.t) ->
    transaction ->
    'arg ->
    ( ('res, 'arity, ([> error ] as 'error)) Model.Query.result,
      'error )
    Lwt_result.t

  val connect :
    t ->
    (transaction -> ('res, 'error) Lwt_result.t) ->
    ('res, ([> error ] as 'error)) Lwt_result.t
end
