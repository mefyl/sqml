type 'a t =
  | Bool : bool t
  | Custom : {
      repr : 'repr t;
      encode : 'a -> ('repr, string) Result.t;
      decode : 'repr -> ('a, string) Result.t;
    }
      -> 'a t
  | Int : int t
  | Text : string t
  | Timestamp : Ptime.t t
  | Varchar : int -> string t
