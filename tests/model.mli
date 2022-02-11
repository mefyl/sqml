open Sqml
include Sqml.Model.S

module T1 : sig
  type t = int * string * string option

  val pp : t Fmt.t
  val equal : t -> t -> bool

  include Table.S with type model = tag and type Meta.mapped = t

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
  type t = int

  val pp : t Fmt.t
  val equal : t -> t -> bool

  include Table.S with type model = tag and type Meta.mapped = t

  module Column : sig
    include module type of Column

    val id : int Expression.column
  end

  val id : int Expression.t
end

module Ts : sig
  include Table.S with type model = tag and type Meta.mapped = Timmy.Time.t

  val time : Timmy.Time.t Expression.t
end

module Configs : sig
  type t = string * Ezjsonm.value

  include Table.S with type model = tag and type Meta.mapped = t

  val id : string Expression.t
  val config : Ezjsonm.value Expression.t
  val equal : t -> t -> bool
  val pp : t Fmt.t
end
