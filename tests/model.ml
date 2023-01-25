open Sqml
open Utils

include Sqml.Model.Make (struct
  let version = Option.value_exn (Semver.from_parts 0 0 0 [] [])
end)

module T1_meta = struct
  let schema = None
  let name = "table1"
  let id = Column.(make ~primary:true "id" Type.Int)
  let name_ = Column.(make "name" Type.Text)
  let description = Column.(make_nullable "description" Type.Text)

  type columns = int -> string -> string option -> unit

  let columns = Column.List.[ id; name_; description ]

  type mapped = int * string * string option [@@deriving eq, show]

  let map Hlist.[ id; name; desc ] = (id, name, desc)
  let unmap (id, name, desc) = Hlist.[ id; name; desc ]
  let constraints Expression.Column.List.[ _id; _name; _description ] = []

  let primary Expression.Column.List.[ id; _name; _desc ] =
    [ Sqml.Table.Primary { column = id } ]

  type indexes =
    (int -> string -> unit, Sqml.Table.Index.unique) Sqml.Table.Index.kind ->
    unit

  let indexes Expression.Column.List.[ id; _name; _desc ] =
    Sqml.Table.Index.List.
      [
        Unique
          {
            columns =
              [
                { column = id; order = Ascending };
                { column = _name; order = Descending };
              ];
            name = "ids";
          };
      ]
end

module T1 = struct
  include BindTable (T1_meta)

  type t = Meta.mapped

  let pp = T1_meta.pp_mapped
  let equal = T1_meta.equal_mapped

  module Column = struct
    include Column

    let Sqml.Expression.Column.List.[ id; name; description ] = columns
  end

  let Sqml.Expression.List.[ id; name; description ] = Expression.columns
end

module T2 = struct
  type t = int [@@deriving eq, show]

  module Meta' = struct
    include Table.Meta (struct
      let name = "table2"

      let id =
        Column.(
          make "id"
            ~foreign_keys:
              [
                Table.Column.Foreign_key.make
                  { table = (module T1); column = T1.Column.id };
              ]
            Type.Int)

      type columns = int -> unit

      let columns = Column.List.[ id ]
    end)

    type mapped = t

    let map Hlist.[ i ] = i
    let unmap i = Hlist.[ i ]
  end

  include BindTable (Meta')

  module Column = struct
    include Column

    let Sqml.Expression.Column.List.[ id ] = columns
  end

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

type value =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of value list
  | `O of (string * value) list
  ]
[@@deriving eq, show]

module Configs = struct
  type t = string * value [@@deriving eq, show]

  module Meta_ = struct
    include Table.Meta (struct
      let name = "configuration"
      let id = Column.(make "id" Type.(Varchar 64))
      let config = Column.(make "config" Type.Json)

      type columns = string -> Ezjsonm.value -> unit

      let columns = Column.List.[ id; config ]
    end)

    type mapped = t

    let map Hlist.[ id; config ] = (id, config)
    let unmap (id, config) = Hlist.[ id; config ]
  end

  include BindTable (Meta_)

  let Sqml.Expression.List.[ id; config ] = Expression.columns
end
