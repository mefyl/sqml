open Sqml

module T1_meta = struct
  let name = "table1"

  let id = { Column.name = "id"; type_ = Type.Int }

  let name_ = { Column.name = "name"; type_ = Type.Text }

  type columns = int * (string * unit)

  let columns = Column.List.(Cons (id, Cons (name_, Empty)))
end

module T1 = struct
  include Model.BindTable (T1_meta)

  module Column = struct
    let Expression.List.(Cons (id, Cons (name, Empty))) = columns
  end
end

module T2_meta = struct
  let name = "table2"

  let id = { Column.name = "id"; type_ = Type.Int }

  type columns = int * unit

  let columns = Column.List.(Cons (id, Empty))
end

module T2 = struct
  include Model.BindTable (T2_meta)

  module Column = struct
    let Expression.List.(Cons (id, Empty)) = columns
  end
end
