open Acid_lwt

type error = [ `Sqml_engine of string ]

module type S = sig
  type t
  type transaction

  module Model : Model.S

  val query :
    transaction ->
    ?tags:Logs.Tag.set ->
    ('t, 'arity) Model.Query.t ->
    ( ('t, 'arity, ([> error ] as 'error)) Root.Model.result,
      'error )
    Lwt_result.t

  type ('args, 'tag, 'res, 'arity) prepared

  val dialect : t -> (module Dialect.S)

  val prepare :
    'args Expression.Type.List.t ->
    (('args, 'tag) Expression.List.t -> ('res, 'arity) Model.Query.t) ->
    ('args, 'tag, 'res, 'arity) prepared

  val run :
    ('args, 'tag, 'res, 'arity) prepared ->
    transaction ->
    ?tags:Logs.Tag.set ->
    'args Hlist.t ->
    ( ('res, 'arity, ([> error ] as 'error)) Root.Model.result,
      'error )
    Lwt_result.t

  val connect :
    t ->
    ?tags:Logs.Tag.set ->
    (transaction -> ('res, 'error) Lwt_result.t) ->
    ('res, ([> error ] as 'error)) Lwt_result.t

  val close : t -> unit Lwt.t
  val schema : t -> (Table.Schema.t list, [> error ]) Lwt_result.t
end

let diff (type t) (module E : S with type t = t) (t : t) =
  let open Let.Syntax2 (Lwt_result) in
  let+ meta = E.schema t in
  Model.diff (E.dialect t) (module E.Model) meta
