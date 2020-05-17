open Acid
open Sqml

include Sqml.Model.Make ()

module T1_meta = struct
  let name = "table1"

  let id = { Column.name = "id"; type_ = Type.Int }

  let name_ = { Column.name = "name"; type_ = Type.Text }

  type columns = int * (string * unit)

  let columns = Column.List.(Cons (id, Cons (name_, Empty)))

  type mapped = int * string

  let map (id, (name, ())) = (id, name)

  let unmap (id, name) = (id, (name, ()))
end

module T1 = struct
  include BindTable (T1_meta)

  module Column = struct
    let Expression.List.(Cons (id, Cons (name, Empty))) = columns
  end
end

module T2_meta = struct
  let name = "table2"

  let id = { Column.name = "id"; type_ = Type.Int }

  type columns = int * unit

  let columns = Column.List.(Cons (id, Empty))

  type mapped = columns

  let map = Fn.id

  let unmap = Fn.id
end

module T2 = struct
  include BindTable (T2_meta)

  module Column = struct
    let Expression.List.(Cons (id, Empty)) = columns
  end
end
