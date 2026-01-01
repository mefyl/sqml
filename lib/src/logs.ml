include Stere_lwt

let src = Src.create ~doc:"SQML" "sqml"

include Lwt.Make (struct
  include (val Logs_lwt.src_log src)

  let msg level msgf =
    let msgf m = msgf (fun ?header ?tags fmt -> m ?header ?tags fmt) in
    msg level msgf

  let kmsg k level msgf = kmsg ?over:None k level msgf
end)

module Sync = Sync.Make ((val src_log src))
