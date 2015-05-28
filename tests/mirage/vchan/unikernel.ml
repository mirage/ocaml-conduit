open Lwt
open Printf

module Server (C: V1_LWT.CONSOLE) = struct

  let conduit = Conduit_mirage.empty

  let rec read_all c t =
    Vchan_xen.read t >>= function
    |`Eof     -> C.log c "EOF"; OS.Time.sleep 5.
    |`Error _ -> C.log c "ERR"; OS.Time.sleep 5.
    |`Ok buf   ->
      C.log c (Cstruct.to_string buf);
      read_all c t

  let start c =
    Conduit_mirage.with_vchan conduit (module OS.Xs)
      (module Vchan_xen) "foo_server" >>= fun conduit ->
    C.log_s c "Server initialising" >>= fun () ->
    Conduit_mirage.listen conduit (`Vchan `Domain_socket) 
      (fun _flow -> C.log_s c "Got a new flow!")

end

module Client (C: V1_LWT.CONSOLE) = struct

  let conduit = Conduit_mirage.empty

  let start c =
    OS.Time.sleep 2.0 >>= fun () ->
    Conduit_mirage.with_vchan conduit (module OS.Xs)
      (module Vchan_xen) "foo_client" >>= fun conduit ->
    C.log_s c "Connecting..." >>= fun () ->
    let endp = match Vchan.Port.of_string "flibble" with
      | `Ok port -> `Vchan (`Domain_socket ("foo_server", port))
      | `Error e -> failwith e
    in
    Conduit_mirage.sexp_of_client endp 
    |> Sexplib.Sexp.to_string_hum
    |> sprintf "Endpoint: %s"
    |> C.log_s c

(* 
    C.log_s c "Client connected" >>= fun () ->
    let rec write num =
      let buf = Io_page.(to_cstruct (get 1)) in
      let s = sprintf "num is %d" num in
      let len = String.length s in
      Cstruct.blit_from_string s 0 buf 0 len;
      let buf = Cstruct.sub buf 0 len in
      Vchan_xen.write flow buf
      >>= function
      |`Eof -> C.log c "EOF"; OS.Time.sleep 5.
      |`Error _ -> C.log c "ERR"; OS.Time.sleep 5.
      |`Ok () -> OS.Time.sleep 0.1 >>= fun () -> write (num+1)
    in
    write 0
*)

end
