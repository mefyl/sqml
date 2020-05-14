type 't t = {
  name : string;
  type_ : 't Type.t;
}

module List = S_list.Make (struct
  type nonrec 'a t = 'a t
end)
