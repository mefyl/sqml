include module type of Expression_intf

include Interface

val make_column :
  selectable:string -> name:string -> 't Type.t -> ('t, 'tag) column

val map_name : (string -> string) -> ('t, 'tag) t -> ('t, 'tag') t
