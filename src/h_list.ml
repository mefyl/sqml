type _ t =
  | ( :: ) : 'a * 'b t -> ('a * 'b) t
  | [] : unit t

let rec to_pairs : type pairs. pairs t -> pairs = function
  | hd :: tl -> (hd, to_pairs tl)
  | [] -> ()
