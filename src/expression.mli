include module type of Expression_intf

include Interface

val make_column : string -> 't Column.t -> ('t, 'tag) t

val map_name : (string -> string) -> ('t, 'tag) t -> ('t, 'tag') t
