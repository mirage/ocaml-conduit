(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
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

open Lwt
open Conduit_lwt_server

let _ = Ssl.init ()

let chans_of_fd sock =
  let shutdown () = Lwt_ssl.ssl_shutdown sock in
  let close () = Lwt_ssl.close sock in
  let oc = Lwt_io.make ~mode:Lwt_io.output ~close:shutdown (Lwt_ssl.write_bytes sock) in
  let ic = Lwt_io.make ~mode:Lwt_io.input ~close (Lwt_ssl.read_bytes sock) in
  ((Lwt_ssl.get_fd sock), ic, oc)

module Client = struct
  (* SSL TCP connection *)
  let t = Ssl.create_context Ssl.SSLv23 Ssl.Client_context
  let () = Ssl.disable_protocols t [Ssl.SSLv23]

  let connect ?(ctx=t) ?src sa =
    with_socket sa (fun fd ->
        let () =
          match src with
          | None -> ()
          | Some src_sa -> Lwt_unix.bind fd src_sa
        in
        Lwt_unix.connect fd sa >>= fun () ->
        Lwt_ssl.ssl_connect fd ctx >>= fun sock ->
        return (chans_of_fd sock)
      )
end

module Server = struct

  let t = Ssl.create_context Ssl.SSLv23 Ssl.Server_context
  let () = Ssl.disable_protocols t [Ssl.SSLv23]

  let accept ?(ctx=t) fd =
    Lwt_unix.accept fd >>= fun (afd, _) ->
    Lwt.try_bind (fun () -> Lwt_ssl.ssl_accept afd ctx)
      (fun sock -> return (chans_of_fd sock))
      (fun exn -> Lwt_unix.close afd >>= fun () -> fail exn)

  let listen ?(ctx=t) ?(nconn=20) ?password ~certfile ~keyfile sa =
    let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sa) Unix.SOCK_STREAM 0 in
    Lwt_unix.(setsockopt fd SO_REUSEADDR true);
    Lwt_unix.bind fd sa;
    Lwt_unix.listen fd nconn;
    (match password with
     | None -> ()
     | Some fn -> Ssl.set_password_callback ctx fn);
    Ssl.use_certificate ctx certfile keyfile;
    Lwt_unix.set_close_on_exec fd;
    fd

  let init ?ctx ?(nconn=20) ?password ~certfile ~keyfile
    ?(stop = fst (Lwt.wait ())) ?timeout sa callback =
    let s = listen ?ctx ~nconn ?password ~certfile ~keyfile sa in
    let cont = ref true in
    async (fun () ->
      stop >>= fun () ->
      cont := false;
      return_unit
    );
    let rec loop () =
      if not !cont then return_unit
      else (
        Lwt.catch
          (fun () -> accept ?ctx s >|= process_accept ~timeout callback)
          (function
            | Lwt.Canceled -> cont := false; return_unit
            | _ -> Lwt_unix.yield ())
        >>= loop
      )
    in
    loop ()

end

(* Should this be deprecated? Not really ssl related in any way *)
let close = Conduit_lwt_server.close
