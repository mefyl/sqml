type 'a t =
  | Bool : bool t
  | Int : int t
  | Text : string t
  | Timestamp : Ptime.t t
  | Varchar : int -> string t
