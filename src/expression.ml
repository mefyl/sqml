open Acid
include Expression_intf

module Column = struct
  module Type = struct
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
  end

  type ('t, 'tag) t = {
    selectable : string;
    name : string;
    type_ : 't Type.t;
  }

  module List = Hlist.Make2 (struct
    type nonrec ('t, 'tag) t = ('t, 'tag) t
  end)
end

type free = unit

type 'a type_ =
  | Bool : bool type_
  | Date : Timmy.Date.t type_
  | Int : int type_
  | Nullable : 't type_ -> 't option type_
  | String : string type_
  | Time : Timmy.Time.t type_
  | Custom : {
      repr : 'repr type_;
      encode : 'a -> ('repr, string) Result.t;
      decode : 'repr -> ('a, string) Result.t;
    }
      -> 'a type_
  | Empty : unit Hlist.t type_
  | Pair : 'a type_ * 'b Hlist.t type_ -> ('a -> 'b) Hlist.t type_

type ('t, 'tag) t =
  | Column : ('t, 'tag) Column.t -> ('t, 'tag) t
  | Comparison : (comparison * ('t, 'tag) t * ('t, 'tag) t) -> (bool, 'tag) t
  | Concat : (string, 'tag) t * (string, 'tag) t -> (string, 'tag) t
  | Const : {
      type_ : 't type_;
      value : 't;
    }
      -> ('t, 'tag) t
  | Logic : (logic * (bool, 'tag) t * (bool, 'tag) t) -> (bool, 'tag) t
  | Map : {
      decode : 'mapped -> 't;
      encode : 't -> 'mapped;
      mapped : ('mapped, 'tag) t;
    }
      -> ('t, 'tag) t
  | Null : ('t option, 'tag) t
  | Var : 't type_ -> ('t, 'tag) t
  | ( :: ) : ('a, 'tag) t * ('b Hlist.t, 'tag) t -> (('a -> 'b) Hlist.t, 'tag) t
  | [] : (unit Hlist.t, 'tag) t

type ('t, 'tag) option =
  | None : (unit, 'tag) option
  | Some : ('t, 'tag) t -> ('t, 'tag) option

module List = Hlist.Make2 (struct
  type nonrec ('t, 'tag) t = ('t, 'tag) t
end)

let const type_ value = Const { type_; value }

let null = Null

let var t = Var t

let make_column ~selectable ~name type_ = Column.{ selectable; name; type_ }

let empty_list = []

let column c = Column c

let int = Int

let map ~decode ~encode mapped = Map { decode; encode; mapped }

let string = String

let some t = Nullable t

let rec map_name :
    type a tag tag'. (string -> string) -> (a, tag) t -> (a, tag') t =
 fun f -> function
  | Column ({ selectable; _ } as pl) ->
    Column { pl with selectable = f selectable }
  | Comparison (op, l, r) -> Comparison (op, map_name f l, map_name f r)
  | Concat (l, r) -> Concat (map_name f l, map_name f r)
  | Const _ as e -> e
  | Logic (op, l, r) -> Logic (op, map_name f l, map_name f r)
  | Map ({ mapped; _ } as pl) -> Map { pl with mapped = map_name f mapped }
  | Null as e -> e
  | Var _ as e -> e
  | a :: b -> map_name f a :: map_name f b
  | [] as e -> e

type 'tag assignment =
  | Set : {
      column : ('t, 'tag) Column.t;
      value : ('t, 'tag) t;
    }
      -> 'tag assignment

module Infix = struct
  let ( && ) l r = Logic (And, l, r)

  let ( := ) column value = Set { column; value }

  let ( < ) l r = Comparison (Lt, l, r)

  let ( <= ) l r = Comparison (Lte, l, r)

  let ( = ) l r = Comparison (Eq, l, r)

  let ( > ) l r = Comparison (Gt, l, r)

  let ( >= ) l r = Comparison (Gte, l, r)

  let ( @ ) a b = a :: b

  let ( ^ ) a b = Concat (a, b)

  let ( || ) l r = Logic (And, l, r)
end

include Infix
