module Make (T : sig
  type 'a t
end) =
struct
  type 't t =
    | Cons : 'hd T.t * 'tl t -> ('hd * 'tl) t
    | Empty : unit t
end
