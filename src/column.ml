type 't t = {
  name : string;
  type_ : 't Type.t;
  primary : bool; (* foreign : Table.foreign_key option; *)
}

let make ?primary name type_ =
  { name; type_; primary = Option.value ~default:false primary }

module List = S_list.Make (struct
  type nonrec 'a t = 'a t
end)
