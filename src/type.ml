type 'a t =
  | Bool : bool t
  | Custom : {
      repr : 'repr t;
      encode : 'a -> ('repr, string) Result.t;
      decode : 'repr -> ('a, string) Result.t;
    }
      -> 'a t
  | Date : Timmy.Date.t t
  | Int : int t
  | Nullable : 't t -> 't option t
  | Text : string t
  | Time : Timmy.Time.t t
  | Varchar : int -> string t

type 'a extended =
  | Atomic : 'a t -> 'a extended
  | Custom : {
      repr : 'repr extended;
      encode : 'a -> ('repr, string) Result.t;
      decode : 'repr -> ('a, string) Result.t;
    }
      -> 'a extended
  | Empty : unit H_list.t extended
  | Pair : 'a extended * 'b H_list.t extended -> ('a -> 'b) H_list.t extended
