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

let _ = Ssl.init ()

let chans_of_fd sock =
  let ic = Lwt_ssl.in_channel_of_descr sock in
  let oc = Lwt_ssl.out_channel_of_descr sock in
  (ic, oc)

let close (ic,oc) =
  let _ = try_lwt Lwt_io.close oc with _ -> return () in
  try_lwt Lwt_io.close ic with _ -> return ()

module Client = struct
  (* SSL TCP connection *)
  let t = Ssl.create_context Ssl.TLSv1 Ssl.Client_context

  let connect sa =
    let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sa) Unix.SOCK_STREAM 0 in
    lwt () = Lwt_unix.connect fd sa in
    lwt sock = Lwt_ssl.ssl_connect fd t in
    return (chans_of_fd sock)
end

module Server = struct

  let t = Ssl.create_context Ssl.TLSv1 Ssl.Server_context

  let accept fd =
    lwt (afd,sockaddr) = Lwt_unix.accept fd in
    lwt sock = Lwt_ssl.ssl_accept afd t in
    return (sockaddr,chans_of_fd sock)

  let listen ?(nconn=20) ?password ~certfile ~keyfile sa =
    let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sa) Unix.SOCK_STREAM 0 in
    Lwt_unix.(setsockopt fd SO_REUSEADDR true);
    Lwt_unix.bind fd sa;
    Lwt_unix.listen fd nconn;
    (match password with
     | None -> ()
     | Some fn -> Ssl.set_password_callback t fn);
    Ssl.use_certificate t certfile keyfile;
    fd

  let process_accept ~timeout callback (sockaddr,(ic,oc)) =
    let c = callback sockaddr ic oc in
    let events = match timeout with
      | None -> [c]
      | Some t -> [c; (Lwt_unix.sleep (float_of_int t)) ] in
    let _ = Lwt.pick events >>= fun () -> close (ic,oc) in
    return ()

  let init ?(nconn=20) ?password ~certfile ~keyfile ?timeout sa callback =
    let s = listen ~nconn ?password ~certfile ~keyfile sa in
    let cont = ref true in
    while_lwt !cont do
      try_lwt begin
        accept s >>= process_accept ~timeout callback
      end with 
       | Lwt.Canceled -> cont := false; return ()
       | _ -> return ()
    done
end
