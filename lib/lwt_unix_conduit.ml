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
open Sexplib.Std

type +'a io = 'a Lwt.t
type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel

type server_mode = [
  | `SSL of [ `Crt_file_path of string ] * [ `Key_file_path of string ]
  | `TCP
] with sexp

module LUN = Lwt_unix_net

let connect ~mode ~host ~service () =
  lwt sa = LUN.build_sockaddr host service in
IFDEF HAVE_LWT_SSL THEN
  match mode with
  | `SSL -> Lwt_unix_net_ssl.connect sa
  | `TCP -> LUN.Tcp_client.connect sa
ELSE
  match mode with
  | `SSL -> fail (Failure "No SSL support compiled into Conduit")
  | `TCP -> LUN.Tcp_client.connect sa
END

let serve ~mode ~sockaddr ?timeout callback =
IFDEF HAVE_LWT_SSL THEN
  match mode with
  | `TCP -> LUN.Tcp_server.init ~sockaddr ?timeout callback
  | `SSL (`Crt_file_path certfile, `Key_file_path keyfile) -> 
    Lwt_unix_net_ssl.init ~certfile ~keyfile ?timeout sockaddr callback
ELSE
  match mode with
  | `SSL -> fail (Failure "No SSL support compiled into Conduit")
  | `TCP -> LUN.Tcp_client.connect sa
END

let close_in ic =
  ignore_result (try_lwt Lwt_io.close ic with _ -> return ())

let close_out oc =
  ignore_result (try_lwt Lwt_io.close oc with _ -> return ())

let close' ic oc =
  try_lwt Lwt_io.close oc with _ -> return () >>= fun () ->
    try_lwt Lwt_io.close ic with _ -> return ()

let close ic oc =
  ignore_result (close' ic oc)
