include module type of Expression_intf

include Interface

val make_column :
  selectable:string -> name:string -> 't Column.Type.t -> ('t, 'tag) Column.t

val map_name : (string -> string) -> ('t, 'tag) t -> ('t, 'tag') t
