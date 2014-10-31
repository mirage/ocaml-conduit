open Lwt
open V1_LWT
open Printf

let red fmt    = sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = sprintf ("\027[36m"^^fmt^^"\027[m")

let domain = "anil.recoil.org"
let uri = Uri.of_string "http://anil.recoil.org"
let ns = Ipaddr.V4.of_string_exn "10.11.12.1"

module Client (C:CONSOLE) (S:STACKV4) = struct

  module DNS = Dns_resolver_mirage.Make(OS.Time)(S)
  module RES = Conduit_resolver_mirage.Make(DNS)
  module CON = Conduit_mirage.Make(S)(Conduit_localhost)

  let start c stack =
    C.log_s c "Starting to resolve in 3s..." >>= fun () ->
    OS.Time.sleep 3.0 >>= fun () ->
    RES.system ~ns ~stack ()
    >>= fun res ->
    Conduit_resolver_lwt.resolve_uri ~uri res
    >>= fun endp ->
    lwt ctx = CON.init ~stack () in
    CON.endp_to_client ~ctx endp
    >>= fun client ->
    C.log_s c (Sexplib.Sexp.to_string_hum (Conduit.sexp_of_endp endp))
    >>= fun () ->
    lwt (conn, ic, oc) = CON.connect ~ctx client in
    let page = Io_page.(to_cstruct (get 1)) in
    let http_get = "GET / HTTP/1.1\nHost: anil.recoil.org\n\n" in
    Cstruct.blit_from_string http_get 0 page 0 (String.length http_get);
    let buf = Cstruct.sub page 0 (String.length http_get) in
    CON.Flow.write oc buf >>= function
    | `Eof -> C.log_s c "EOF on write"
    | `Error _ -> C.log_s c "ERR on write"
    | `Ok buf -> begin
      CON.Flow.read ic >>= function
      | `Eof -> C.log_s c "EOF"
      | `Error _ -> C.log_s c "ERR"
      | `Ok buf -> C.log_s c (sprintf "OK\n%s\n" (Cstruct.to_string buf))
    end
end
