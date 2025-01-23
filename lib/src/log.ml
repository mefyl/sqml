let src = Logs.Src.create ~doc:"SQML" "sqml"

include (val Logs_lwt.src_log src)
module Sync = (val Logs.src_log src)
