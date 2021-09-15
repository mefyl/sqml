open Sqml

include Sqml.Model.S

module T1 : sig
  include Table.S with type model = tag and type Meta.mapped = int * string

  val id : int Expression.t

  val name : string Expression.t
end

module T2 : sig
  include Table.S with type model = tag

  val id : int Expression.t
end

module Ts : sig
  include Table.S with type model = tag and type Meta.mapped = Timmy.Time.t

  val time : Timmy.Time.t Expression.t
end
