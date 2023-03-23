open Acid
include module type of Expression_intf

module Column : sig
  include module type of Column

  module Type : sig
    include module type of Type

    val equal : 'a t -> 'b t -> bool
    val equal_any : any -> any -> bool
    val equal_any_atomic : any_atomic -> any_atomic -> bool
  end

  type ('t, 'tag) t = private {
    name : string;
    selectable : string;
    type_ : 't Type.t;
  }

  module List : Hlist.S2 with type ('t, 'tag) value = ('t, 'tag) t
end

module Type : sig
  include module type of Type

  val of_column_type : 't Column.Type.t -> 't t
  val pp : 't t Fmt.t
end

type ('t, 'tag) t

type ('t, 'tag) option =
  | None : (unit, 'tag) option
  | Some : ('t, 'tag) t -> ('t, 'tag) option

type 'tag assignment

module List : Hlist.S2 with type ('t, 'tag) value = ('t, 'tag) t

module List_nullable :
  Hlist.S2 with type ('t, 'tag) value = ('t Option.t, 'tag) t

val case :
  ((bool, 'tag) t * ('t Option.t, 'tag) t) list -> ('t Option.t, 'tag) t

val case_else :
  ((bool, 'tag) t * ('t, 'tag) t) list -> ('t, 'tag) t -> ('t, 'tag) t

val now : (Timmy.Time.t, 'tag) t
val to_ast : ('t, 'tag) t -> ast
val to_type : ('t, 'tag) t -> 't Type.t
val of_list : ('l, 'tag) List.t -> ('l Hlist.t, 'tag) t
val to_list : ('l Hlist.t, 'tag) t -> ('l, 'tag) List.t
val coalesce : ('t Option.t, 'tag) t list -> ('t Option.t, 'tag) t

val coalesce_default :
  ('t Option.t, 'tag) t list -> ('t, 'tag) t -> ('t, 'tag) t

val column : ('t, 'tag) Column.t -> ('t, 'tag) t
val const : 't Type.t -> 't -> ('t, 'tag) t

val map :
  decode:('mapped -> ('t, string) Result.t) ->
  encode:('t -> 'mapped) ->
  ('mapped, 'tag) t ->
  ('t, 'tag) t

val null : (_ Option.t, 'tag) t
val is_null : (_ Option.t, 'tag) t -> (bool, 'tag) t
val is_not_null : (_ Option.t, 'tag) t -> (bool, 'tag) t
val some : ('t, 'tag) t -> ('t Option.t, 'tag) t

(* FIXME: private *)
val prepare : 'args Type.List.t -> ('args, 'free) List.t
val assignment_to_asts : 'a assignment -> (string * ast) list

module Infix : sig
  val ( && ) : (bool, 'tag) t -> (bool, 'tag) t -> (bool, 'tag) t
  val ( := ) : ('t, 'tag) Column.t -> ('t, 'tag) t -> 'tag assignment
  val ( < ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t
  val ( <= ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t
  val ( = ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t
  val ( > ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t
  val ( >= ) : ('t, 'tag) t -> ('t, 'tag) t -> (bool, 'tag) t
  val ( ^ ) : (string, 'tag) t -> (string, 'tag) t -> (string, 'tag) t
  val ( || ) : (bool, 'tag) t -> (bool, 'tag) t -> (bool, 'tag) t
  val ( + ) : (int, 'tag) t -> (int, 'tag) t -> (int, 'tag) t
  val ( - ) : (int, 'tag) t -> (int, 'tag) t -> (int, 'tag) t
end

include module type of Infix

module Traverse_ast (M : Monad.S2) : sig
  val traverse_ast : ast -> f:(ast -> (ast, 'b) M.t) -> (ast, 'b) M.t
end

val traverse_ast : ast -> f:(ast -> ast) -> ast

val make_column :
  name:string -> selectable:string -> 't Column.Type.t -> ('t, 'tag) Column.t

val map_name : (string -> string) -> ('t, 'tag) t -> ('t, 'tag') t
val in_ : ('what, 'tag) t -> ('what list, 'tag) t -> (bool, 'tag) t

val select :
  from:(module SELECTABLE with type tag = 'tag) ->
  ?for_:for_ ->
  ?limit:int ->
  ?skip_locked:bool ->
  ?where:(bool, 'tag) t ->
  ('what, 'tag) t ->
  ('what list, 'tag) t

val select_one :
  from:(module SELECTABLE with type tag = 'tag) ->
  ?for_:for_ ->
  ?skip_locked:bool ->
  ?where:(bool, 'tag) t ->
  ('what, 'tag) t ->
  ('what Option.t, 'tag) t

val coerce : ('t, _) t -> ('t, _) t
