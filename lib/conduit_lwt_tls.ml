(*
 * Copyright (c) 2014 Hannes Mehnert <hannes@mehnert.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Lwt.Infix

let _ = Nocrypto_entropy_lwt.initialize ()

module Client = struct
  let connect ?src host sa =
    Conduit_lwt_server.with_socket sa (fun fd ->
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
      )
end

module Server = struct
  let listen backlog sa =
    let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sa) Unix.SOCK_STREAM 0 in
    Lwt_unix.(setsockopt fd SO_REUSEADDR true);
    Lwt_unix.bind fd sa;
    Lwt_unix.listen fd backlog;
    Lwt_unix.set_close_on_exec fd;
    fd

  let accept config s =
    Lwt_unix.accept s >>= fun (fd, _) ->
    Lwt.try_bind (fun () ->
        Tls_lwt.Unix.server_of_fd config fd)
      (fun t ->
         let ic, oc = Tls_lwt.of_t t in
         Lwt.return (fd, ic, oc))
      (fun exn -> Lwt_unix.close fd >>= fun () -> Lwt.fail exn)

  let init ?(backlog=128) ~certfile ~keyfile
      ?(stop = fst (Lwt.wait ())) ?timeout sa callback =
    X509_lwt.private_of_pems ~cert:certfile ~priv_key:keyfile
    >>= fun certificate ->
    let config = Tls.Config.server ~certificates:(`Single certificate) () in
    let s = listen backlog sa in
    let stop' = Lwt.map (fun () -> `Stop) stop in
    let rec loop () =
      let accept = accept config s in
      Lwt.choose [ Lwt.map (fun v -> `Accept v) accept ; stop' ] >>= function
      | `Stop ->
        Lwt.cancel accept;
        Lwt.return_unit
      | `Accept v ->
        Conduit_lwt_server.process_accept ~timeout callback v;
        loop ()
    in
    Lwt.finalize loop (fun () -> Lwt_unix.close s)
end
