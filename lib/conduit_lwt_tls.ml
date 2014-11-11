open Lwt

let _ = Tls_lwt.rng_init ()

module Client = struct
  let connect ?src host sa =
    let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sa) Unix.SOCK_STREAM 0 in
    let () =
      match src with
      | None -> ()
      | Some src_sa -> Lwt_unix.bind fd src_sa
    in
    X509_lwt.authenticator `No_authentication_I'M_STUPID >>= fun authenticator ->
    let config = Tls.Config.client ~authenticator () in
    Lwt_unix.connect fd sa >>= fun () ->
    Tls_lwt.Unix.client_of_fd config ~host fd >|= fun t ->
    let ic, oc = Tls_lwt.of_t t in
    (fd, ic, oc)
end
