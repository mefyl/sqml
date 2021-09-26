open Sqml

include Sqml.Model.S

module T1 : sig
  include
    Table.S
      with type model = tag
       and type Meta.mapped = int * string * string option

  module Column : sig
    include module type of Column

    val id : int Expression.column

    val name : string Expression.column

    val description : string option Expression.column
  end

  val id : int Expression.t

  val name : string Expression.t

  val description : string option Expression.t
end

module T2 : sig
  include Table.S with type model = tag

  val id : int Expression.t
end

module Ts : sig
  include Table.S with type model = tag and type Meta.mapped = Timmy.Time.t

  val time : Timmy.Time.t Expression.t
end

module Configs : sig
  type t = string * Schematic.Json.t

  include Table.S with type model = tag and type Meta.mapped = t

  val id : string Expression.t

  val config : Schematic.Json.t Expression.t

  val equal : t -> t -> bool

  val pp : t Fmt.t
end
