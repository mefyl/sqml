type 'a t =
  | Bool : bool t
  | Custom : {
      repr : 'repr t;
      encode : 'a -> ('repr, string) Result.t;
      decode : 'repr -> ('a, string) Result.t;
    }
      -> 'a t
  | Int : int t
  | Nullable : 't t -> 't option t
  | Text : string t
  | Time : Timmy.Time.t t
  | Varchar : int -> string t
