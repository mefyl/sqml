include module type of Expression_intf

include Interface

val traverse_ast : ast -> f:(ast -> ast) -> ast

val make_column :
  name:string ->
  primary:bool ->
  selectable:string ->
  't Column.Type.t ->
  ('t, 'tag) Column.t

val map_name : (string -> string) -> ('t, 'tag) t -> ('t, 'tag') t

val coerce : ('t, _) t -> ('t, _) t
