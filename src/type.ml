type 'a t =
  | Int : int t
  | Text : string t
  | Varchar : int -> string t
