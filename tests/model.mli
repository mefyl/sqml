open Sqml

include Sqml.Model.S

module T1 : sig
  include Table.S with type model = tag and type Meta.mapped = int * string

  module Column : sig
    val id : int Expression.t

    val name : string Expression.t
  end
end

module T2 : sig
  include Table.S with type model = tag

  module Column : sig
    val id : int Expression.t
  end
end
