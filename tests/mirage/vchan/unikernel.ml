open Lwt
open Printf

module Server (C: V1_LWT.CONSOLE) = struct

  let rec read_all c t =
    Vchan_xen.read t
    >>= function
    |`Eof -> C.log c "EOF"; OS.Time.sleep 5.
    |`Error _ -> C.log c "ERR"; OS.Time.sleep 5.
    |`Ok buf ->
      let s = Cstruct.to_string buf in
      C.log c s;
      read_all c t

  let start c =
    Conduit_xenstore.register "foo_server"
    >>= fun t ->
    C.log_s c "Server initialising" >>= fun () ->
    Conduit_xenstore.listen t
    >>= fun conns ->
    Lwt_stream.iter_p (fun endp ->
      return ()
    ) conns

end

module Client (C: V1_LWT.CONSOLE) = struct

  let start c =
    OS.Time.sleep 2.0 >>= fun () ->
    Conduit_xenstore.register "foo_client" 
    >>= fun t ->
    C.log_s c "Connecting..." >>= fun () ->
    Conduit_xenstore.connect t ~remote_name:"foo_server" ~port:"flibble"
    >>= fun endp ->
    C.log_s c (sprintf "Endpoint: %s"
      (Sexplib.Sexp.to_string_hum (Conduit.sexp_of_endp endp)))
    >>= fun () ->
    return ()
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
