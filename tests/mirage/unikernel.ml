open Lwt
open V1_LWT
open Printf

let red fmt    = sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = sprintf ("\027[36m"^^fmt^^"\027[m")

let domain = "anil.recoil.org"
let uri = Uri.of_string "http://anil.recoil.org"
let ns = Ipaddr.V4.of_string_exn "8.8.8.8"

module Client (C:CONSOLE) (S:STACKV4) = struct

  module U = S.UDPV4
  module DNS = Dns_resolver_mirage.Make(OS.Time)(S)
  module RES = Mirage_resolver.Make(DNS)
  module COND = Mirage_conduit.Make(S)(DNS)

  let start c s =
    Console.log_s c "Starting to resolve in 3s..." >>= fun () ->
    OS.Time.sleep 3.0 >>= fun () ->
    let r = RES.system ~ns s in
    Lwt_conduit_resolver.resolve_uri ~uri r
    >>= fun endp ->
    Console.log_s c (Sexplib.Sexp.to_string_hum (Conduit.sexp_of_endp endp))
    >>= fun () ->
    lwt ctx = COND.init r s in
    lwt (conn, ic, oc) = COND.connect_to_uri ~ctx uri in
    let page = Io_page.(to_cstruct (get 1)) in
    let http_get = "GET / HTTP/1.1\nHost: anil.recoil.org\n\n" in
    Cstruct.blit_from_string http_get 0 page 0 (String.length http_get);
    let buf = Cstruct.sub page 0 (String.length http_get) in
    lwt () = S.TCPV4.write oc buf in
    S.TCPV4.read ic >>= function 
    | `Eof -> Console.log_s c "EOF"
    | `Error _ -> Console.log_s c "ERR"
    | `Ok buf -> Console.log_s c (sprintf "OK\n%s\n" (Cstruct.to_string buf))

end
