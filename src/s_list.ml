module Make2 (T : sig
  type ('a, 'b) t
end) =
struct
  type ('t, 'tag) t =
    | Cons : ('hd, 'tag) T.t * ('tl, 'tag) t -> ('hd * 'tl, 'tag) t
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
