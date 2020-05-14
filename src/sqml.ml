module type Intf = sig
  type 'tag column

  type ('from, 'to_) aliaser

  module type Table = sig
    type tag
  end

  module type T1_meta = sig
    include Table

    val id : tag column
  end

  module type T1 = sig
    include T1_meta

    val alias :
      (module T1_meta with type tag = 'from) ->
      ('from, 'to_) aliaser ->
      (module T1_meta with type tag = 'to_)
  end

  module T1 : T1

  module type T2_meta = sig
    include Table

    val id : tag column
  end

  module type T2 = sig
    include T2_meta

    val alias :
      (module T2_meta with type tag = 'from) ->
      ('from, 'to_) aliaser ->
      (module T2_meta with type tag = 'to_)
  end

  module T2 : T2

  module Join (L : Table) (R : Table) : sig
    type tag

    val left : (L.tag, tag) aliaser

    val right : (R.tag, tag) aliaser
  end

  type 'tag table = (module Table with type tag = 'tag)

  val select : 'tag column list -> 'tag table -> unit
end

module Impl : Intf = struct
  type 'tag column = Column

  type ('from, 'to_) aliaser = Aliaser

  module type Table = sig
    type tag
  end

  module type T1_meta = sig
    include Table

    val id : tag column
  end

  module type T1 = sig
    include T1_meta

    val alias :
      (module T1_meta with type tag = 'from) ->
      ('from, 'to_) aliaser ->
      (module T1_meta with type tag = 'to_)
  end

  module T1 : T1 = struct
    type tag = unit

    let id = Column

    let alias :
        type from to_.
        (module T1_meta with type tag = from) ->
        (from, to_) aliaser ->
        (module T1_meta with type tag = to_) =
     fun _ _ ->
      ( module struct
        type tag = to_

        let id = Column
      end )
  end

  module type T2_meta = sig
    include Table

    val id : tag column
  end

  module type T2 = sig
    include T2_meta

    val alias :
      (module T2_meta with type tag = 'from) ->
      ('from, 'to_) aliaser ->
      (module T2_meta with type tag = 'to_)
  end

  module T2 : T2 = struct
    type tag

    let id = Column

    let alias :
        type from to_.
        (module T2_meta with type tag = from) ->
        (from, to_) aliaser ->
        (module T2_meta with type tag = to_) =
     fun _ _ ->
      ( module struct
        type tag = to_

        let id = Column
      end )
  end

  module Join (L : Table) (R : Table) = struct
    type tag

    let left : (L.tag, tag) aliaser = Aliaser

    let right : (R.tag, tag) aliaser = Aliaser
  end

  type 'tag table = (module Table with type tag = 'tag)

  let select : 'tag column list -> 'tag table -> unit = fun _ _ -> ()
end

let () = Impl.select [ Impl.T1.id ] (module Impl.T1)

let () =
  let module J = Impl.Join (Impl.T1) (Impl.T2) in
  let left = Impl.T1.alias (module Impl.T1) J.left
  and right = Impl.T2.alias (module Impl.T2) J.right in
  let module Left = (val left) in
  let module Right = (val right) in
  Impl.select [ Left.id; Right.id ] (module J)
