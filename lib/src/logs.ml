include Stere_lwt

let src = Src.create ~doc:"SQML" "sqml"

include (val Stere_lwt.Lwt.make src)
module Sync = (val Sync.make src)
