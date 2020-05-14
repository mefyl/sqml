open Sqml

module T1 : sig
  include Table.S

  module Column : sig
    val id : int Expression.t

    val name : string Expression.t
  end
end

module T2 : sig
  include Table.S

  module Column : sig
    val id : int Expression.t
  end
end
