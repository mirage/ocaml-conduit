open Lwt
open V1_LWT
open Printf

let red fmt    = sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = sprintf ("\027[36m"^^fmt^^"\027[m")

let uri = Uri.of_string "http://localhost"

module Client (C:CONSOLE) (S:STACKV4) = struct

  let conduit = Conduit_mirage.empty

  let callback c _flow =
    C.log_s c "Connection!"

  let start c stack =
    let r = Resolver_mirage.localhost in
    Conduit_mirage.with_tcp conduit (module S) stack >>= fun conduit ->
    Resolver_lwt.resolve_uri ~uri r >>= fun endp ->
    Conduit_mirage.server endp >>= fun mode ->
    let endp = Sexplib.Sexp.to_string_hum (Conduit.sexp_of_endp endp) in
    C.log_s c endp >>= fun () ->
    Conduit_mirage.listen conduit mode (callback c)

end
