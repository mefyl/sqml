module type S2 = sig
  type ('t, 'tag) value

  type ('t, 'tag) t =
    | Cons : ('hd, 'tag) value * ('tl, 'tag) t -> ('hd * 'tl, 'tag) t
    | Empty : (unit, 'tag) t

  val length : _ t -> int
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
end

module Make (T : sig
  type 'a t
end) =
struct
  type 't t =
    | Cons : 'hd T.t * 'tl t -> ('hd * 'tl) t
    | Empty : unit t

  let length l =
    let rec loop : type l. int -> l t -> int =
     fun acc -> function
      | Cons (_, tl) -> loop (acc + 1) tl
      | Empty -> acc
    in
    loop 0 l
end
