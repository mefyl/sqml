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

  let description = Column.make_nullable "description" Type.Text

  type columns = int * (string * (string option * unit))

  let columns = Column.List.(Cons (id, Cons (name_, Cons (description, Empty))))

  type mapped = int * string * string option

  let map (id, (name, (desc, ()))) = (id, name, desc)

  let unmap (id, name, desc) = (id, (name, (desc, ())))

  let constraints
      Expression.Column_list.(
        Cons (_id, Cons (_name, Cons (_description, Empty)))) =
    []
end

module T1 = struct
  include BindTable (T1_meta)

  let Sqml.Expression.List.(Cons (id, Cons (name, Cons (description, Empty)))) =
    Expression.columns

  let Sqml.Expression.Column_list.(Cons (id_column, _)) = Column.columns
end

module T2_meta = Table.Meta (struct
  let name = "table2"

  let id =
    Column.make "id"
      ~foreign_keys:
        [
          Table.Column.Reference { table = (module T1); column = T1.id_column };
        ]
      Type.Int

  type columns = int * unit

  let columns = Column.List.(Cons (id, Empty))
end)

module T2 = struct
  include BindTable (T2_meta)

  let Sqml.Expression.List.(Cons (id, Empty)) = Expression.columns
end

module Ts = struct
  module Meta_ = struct
    include Table.Meta (struct
      let name = "ts"

      let time = Column.make "time" Type.Time

      type columns = Timmy.Time.t * unit

      let columns = Column.List.(Cons (time, Empty))
    end)

    type mapped = Timmy.Time.t

    let map (time, ()) = time

    let unmap time = (time, ())
  end

  include BindTable (Meta_)

  let Sqml.Expression.List.(Cons (time, Empty)) = Expression.columns
end
