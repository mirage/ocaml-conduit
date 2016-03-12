open Lwt
open Printf

let conduit = Conduit_mirage.empty
let vchan = Conduit_mirage.vchan (module Vchan_xen) 
let xs = Conduit_mirage.xs (module OS.Xs) 

module Server (C: V1_LWT.CONSOLE) = struct

  let start c =
    Conduit_mirage.with_vchan conduit xs vchan "foo_server" >>= fun t ->
    C.log_s c "Server initialising" >>= fun () ->
    let callback flow =
      C.log_s c "Got a new flow!"
      >>= fun () ->
      let rec loop () =
        Conduit_mirage.Flow.read flow
        >>= fun res ->
        match res with
        | `Ok buf ->
           C.log_s c (Printf.sprintf "Received: %s" (Cstruct.to_string buf)) >>= loop
        | `Eof -> Lwt.return ()
        | `Error e -> C.log_s c "Got error"
      in loop ()
    in
    Conduit_mirage.listen t (`Vchan `Domain_socket) callback

end

module Client (C: V1_LWT.CONSOLE) = struct

  let conduit = Conduit_mirage.empty

  let start c =
    OS.Time.sleep 2.0 >>= fun () ->
    Conduit_mirage.with_vchan conduit xs vchan "foo_client" >>= fun t ->
    C.log_s c "Connecting..." >>= fun () ->
    let client = match Vchan.Port.of_string "flibble" with
      | `Ok port -> `Vchan (`Domain_socket ("foo_server", port))
      | `Error e -> failwith e
    in
    Conduit_mirage.connect t client >>= fun flow ->
    Conduit_mirage.sexp_of_client client
    |> Sexplib.Sexp.to_string_hum
    |> sprintf "Endpoint: %s"
    |> C.log_s c >>= fun () ->

    C.log_s c "Client connected" >>= fun () ->
    let rec write num =
      let buf = Io_page.(to_cstruct (get 1)) in
      let s = sprintf "num is %d" num in
      let len = String.length s in
      Cstruct.blit_from_string s 0 buf 0 len;
      let buf = Cstruct.sub buf 0 len in
      Conduit_mirage.Flow.write flow buf
      >>= function
      |`Eof -> C.log c "EOF"; OS.Time.sleep 5.
      |`Error _ -> C.log c "ERR"; OS.Time.sleep 5.
      |`Ok () -> OS.Time.sleep 0.1 >>= fun () -> write (num+1)
    in
    write 0

end
