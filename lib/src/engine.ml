open Acid

type error = [ `Sqml_engine of string ]

module type S = sig
  type t
  type transaction

  module Model : Model.S

  val query :
    transaction ->
    ('t, 'arity) Model.Query.t ->
    ( ('t, 'arity, ([> error ] as 'error)) Root.Model.result,
      'error )
    Lwt_result.t

  type ('args, 'tag, 'res, 'arity) prepared

  val prepare :
    'args Expression.Type.List.t ->
    (('args, 'tag) Expression.List.t -> ('res, 'arity) Model.Query.t) ->
    ('args, 'tag, 'res, 'arity) prepared

  val run :
    ('args, 'tag, 'res, 'arity) prepared ->
    transaction ->
    'args Hlist.t ->
    ( ('res, 'arity, ([> error ] as 'error)) Root.Model.result,
      'error )
    Lwt_result.t

  val connect :
    t ->
    (transaction -> ('res, 'error) Lwt_result.t) ->
    ('res, ([> error ] as 'error)) Lwt_result.t
end
