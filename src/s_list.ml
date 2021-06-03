module type S2 = sig
  type ('t, 'tag) value

  type ('t, 'tag) t =
    | Cons : ('hd, 'tag) value * ('tl, 'tag) t -> ('hd * 'tl, 'tag) t
    | Empty : (unit, 'tag) t

  val length : _ t -> int

  type 'res mapper = { f : 't 'tag. ('t, 'tag) value -> 'res option }

  val map : ('t, 'tag) t -> f:'res mapper -> 'res list
end

module Make2 (T : sig
  type ('a, 'b) t
end) : S2 with type ('t, 'tag) value = ('t, 'tag) T.t = struct
  type ('t, 'tag) value = ('t, 'tag) T.t

  type ('t, 'tag) t =
    | Cons : ('hd, 'tag) value * ('tl, 'tag) t -> ('hd * 'tl, 'tag) t
    | Empty : (unit, 'tag) t

  let length l =
    let rec loop : type l tag. int -> (l, tag) t -> int =
     fun acc -> function
      | Cons (_, tl) -> loop (acc + 1) tl
      | Empty -> acc
    in
    loop 0 l

  type 'res mapper = { f : 't 'tag. ('t, 'tag) value -> 'res option }

  let map l ~(f : 'res mapper) =
    let rec map : type a tag. (a, tag) t -> 'res list = function
      | Cons (hd, tl) -> (
        match f.f hd with
        | Some value -> value :: map tl
        | None -> map tl)
      | Empty -> []
    in
    map l
end

module type S = sig
  type 't value

  type 't t =
    | Cons : 'hd value * 'tl t -> ('hd * 'tl) t
    | Empty : unit t

  val length : _ t -> int

  type 'res mapper = { f : 't 'tag. 't value -> 'res option }

  val map : 't t -> f:'res mapper -> 'res list
end

module Make (T : sig
  type 'a t
end) : S with type 'a value = 'a T.t = struct
  type 't value = 't T.t

  type 't t =
    | Cons : 'hd value * 'tl t -> ('hd * 'tl) t
    | Empty : unit t

  let length l =
    let rec loop : type l. int -> l t -> int =
     fun acc -> function
      | Cons (_, tl) -> loop (acc + 1) tl
      | Empty -> acc
    in
    loop 0 l

  type 'res mapper = { f : 't. 't value -> 'res option }

  let map l ~(f : 'res mapper) =
    let rec map : type a. a t -> 'res list = function
      | Cons (hd, tl) -> (
        match f.f hd with
        | Some value -> value :: map tl
        | None -> map tl)
      | Empty -> []
    in
    map l
end
