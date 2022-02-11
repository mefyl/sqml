open Acid

let src = Logs.Src.create ~doc:"SQML" "sqml"
let info f = Logs_lwt.info ~src f |> Lwt.map ~f:Result.return
let debug f = Logs_lwt.debug ~src f |> Lwt.map ~f:Result.return
let warn f = Logs_lwt.warn ~src f |> Lwt.map ~f:Result.return
let err f = Logs_lwt.err ~src f |> Lwt.map ~f:Result.return
