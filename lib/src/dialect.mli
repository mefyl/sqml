include module type of Dialect_intf

val json_of_sqml : 't Expression.Type.t -> 't -> Ezjsonm.value

val sqml_of_json :
  't Expression.Type.t -> Ezjsonm.value -> ('t, string) Result.t

module Postgresql : S

val postgresql : t

module Sqlite : S

val sqlite : t
