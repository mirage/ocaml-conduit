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
module LUNS = Lwt_unix_net_ssl

let build_sockaddr host port =
  let open Lwt_unix in
  getaddrinfo host (string_of_int port) [AI_PROTOCOL port]
  >>= function
  | [] ->
    fail (Invalid_argument (Printf.sprintf "No socket address for %s/%d" host port))
  | ai::_ -> return ai.ai_addr

let connect ~mode ~host ~service () =
  lwt sa = LUN.build_sockaddr host service in
  match mode with
  | `SSL -> LUNS.connect sa
  | `TCP -> LUN.Tcp_client.connect sa

let serve ~mode ~sockaddr ?timeout callback =
  match mode with
  | `TCP -> LUN.Tcp_server.init ~sockaddr ?timeout callback
  | `SSL (`Crt_file_path certfile, `Key_file_path keyfile) -> 
    LUNS.init ~certfile ~keyfile ?timeout sockaddr callback

let close_in ic =
  ignore_result (try_lwt Lwt_io.close ic with _ -> return ())

let close_out oc =
  ignore_result (try_lwt Lwt_io.close oc with _ -> return ())

let close' ic oc =
  try_lwt Lwt_io.close oc with _ -> return () >>= fun () ->
    try_lwt Lwt_io.close ic with _ -> return ()

let close ic oc =
  ignore_result (close' ic oc)
