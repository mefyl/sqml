open Acid
open Sqml

include Sqml.Model.Make (struct
  let version = (0, 0, 0)
end)

module T1_meta = struct
  let schema = None

  let name = "table1"

  let id = Column.(make ~primary:true "id" Type.Int)

  let name_ = Column.(make "name" Type.Text)

  let description = Column.(make_nullable "description" Type.Text)

  type columns = int -> string -> string option -> unit

  let columns = Column.List.[ id; name_; description ]

  type mapped = int * string * string option

  let map Hlist.[ id; name; desc ] = (id, name, desc)

  let unmap (id, name, desc) = Hlist.[ id; name; desc ]

  let constraints Expression.Column.List.[ _id; _name; _description ] = []
end

module T1 = struct
  include BindTable (T1_meta)

  module Column = struct
    include Column

    let Sqml.Expression.Column.List.[ id; name; description ] = columns
  end

  let Sqml.Expression.List.[ id; name; description ] = Expression.columns
end

module T2_meta = Table.Meta (struct
  let name = "table2"

  let id =
    Column.(
      make "id"
        ~foreign_keys:
          [
            Table.Column.Reference
              { table = (module T1); column = T1.Column.id };
          ]
        Type.Int)

  type columns = int -> unit

  let columns = Column.List.[ id ]
end)

module T2 = struct
  include BindTable (T2_meta)

  let Sqml.Expression.List.[ id ] = Expression.columns
end

module Ts = struct
  module Meta_ = struct
    include Table.Meta (struct
      let name = "ts"

      let time = Column.(make "time" Type.Time)

      type columns = Timmy.Time.t -> unit

      let columns = Column.List.[ time ]
    end)

    type mapped = Timmy.Time.t

    let map Hlist.[ time ] = time

    let unmap time = Hlist.[ time ]
  end

  include BindTable (Meta_)

  let Sqml.Expression.List.[ time ] = Expression.columns
end
