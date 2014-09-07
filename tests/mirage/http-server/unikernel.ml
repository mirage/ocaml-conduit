open Lwt
open V1_LWT
open Printf

let red fmt    = sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = sprintf ("\027[36m"^^fmt^^"\027[m")

let uri = Uri.of_string "http://localhost"

module Client (C:CONSOLE) (S:STACKV4) = struct

  module CON = Conduit_mirage.Make(S)

  let callback c flow ic oc =
    Console.log_s c "Connection!" >>= fun () ->
    return ()

  let start c s =
    let r = Conduit_resolver_mirage.localhost in
    CON.init s
    >>= fun ctx ->
    Conduit_resolver_lwt.resolve_uri ~uri r
    >>= fun endp ->
    CON.endp_to_server ~ctx endp
    >>= fun mode ->
    Console.log_s c (Sexplib.Sexp.to_string_hum (Conduit.sexp_of_endp endp))
    >>= fun () ->
    CON.serve ~ctx ~mode (callback c)
end
