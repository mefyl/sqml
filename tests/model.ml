open Acid
open Sqml

include Sqml.Model.Make (struct
  let version = (0, 0, 0)
end)

module T1_meta = struct
  let schema = None

  let name = "table1"

  let id = Column.make ~primary:true "id" Type.Int

  let name_ = Column.make "name" Type.Text

  type columns = int * (string * unit)

  let columns = Column.List.(Cons (id, Cons (name_, Empty)))

  type mapped = int * string

  let map (id, (name, ())) = (id, name)

  let unmap (id, name) = (id, (name, ()))
end

module T1 = struct
  include BindTable (T1_meta)

  let Expression.List.(Cons (id, Cons (name, Empty))) = columns
end

module T2_meta = struct
  let schema = None

  let name = "table2"

  let id = Column.make "id" Type.Int

  type columns = int * unit

  let columns = Column.List.(Cons (id, Empty))

  type mapped = columns

  let map = Fn.id

  let unmap = Fn.id
end

module T2 = struct
  include BindTable (T2_meta)

  let Expression.List.(Cons (id, Empty)) = columns
end

module Ts = struct
  module Meta_ = struct
    let schema = None

    let name = "ts"

    let time = Column.make "time" Type.Timestamp

    type columns = Ptime.t * unit

    let columns = Column.List.(Cons (time, Empty))

    type mapped = Ptime.t

    let map (time, ()) = time

    let unmap time = (time, ())
  end

  include BindTable (Meta_)

  let Expression.List.(Cons (time, Empty)) = columns
end
