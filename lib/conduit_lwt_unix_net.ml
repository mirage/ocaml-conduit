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
open Printf

(* Vanilla sockaddr connection *)
module Sockaddr_client = struct
  let connect ?src sa =
    let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sa) Unix.SOCK_STREAM 0 in
    let () = 
      match src with
      | None -> print_endline "no src"; ()
      | Some src_sa -> print_endline "has src"; Lwt_unix.bind fd src_sa
    in
    lwt () = Lwt_unix.connect fd sa in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
    return (fd, ic, oc)

  let close (ic,oc) =
    let _ = try_lwt Lwt_io.close oc with _ -> return () in
    try_lwt Lwt_io.close ic with _ -> return ()
end

module Sockaddr_server = struct

  let close (ic,oc) =
    try_lwt Lwt_io.close oc with _ -> return () >>= fun () ->
    try_lwt Lwt_io.close ic with _ -> return ()

  let init_socket sockaddr =
    Unix.handle_unix_error (fun () ->
      let sock = Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
      Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
      Lwt_unix.bind sock sockaddr;
      Lwt_unix.listen sock 15;
      sock) ()

  let process_accept ?timeout callback (client,_) =
    Lwt_unix.setsockopt client Lwt_unix.TCP_NODELAY true;
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input client in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.output client in
    let c = callback client ic oc in
    let events = match timeout with
      |None -> [c]
      |Some t -> [c; (Lwt_unix.sleep (float_of_int t)) ] in
    let _ = Lwt.pick events >>= fun () -> close (ic,oc) in
    return ()

  let init ~sockaddr ?(stop = fst (Lwt.wait ())) ?timeout callback =
    let cont = ref true in
    let s = init_socket sockaddr in
    async (fun () -> 
      stop >>= fun () -> 
      cont := false; 
      return_unit
    );
    while_lwt !cont do
      Lwt_unix.accept s >>=
      process_accept ?timeout callback
    done
end
