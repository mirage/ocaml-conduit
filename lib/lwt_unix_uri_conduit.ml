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

type +'a io = 'a Lwt.t
type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel

module LUN = Lwt_unix_net
module LUNS = Lwt_unix_net_ssl

let connect_uri ?interrupt uri =
  (match Uri_services.tcp_port_of_uri uri with
    |None -> Lwt.fail (Invalid_argument "unknown scheme")
    |Some p -> Lwt.return (string_of_int p))
  >>= fun service ->
  LUN.build_sockaddr (Uri.host_with_default uri) service >>= fun sa ->
  match Uri.scheme uri with
  | Some "https" -> LUNS.connect sa
  | Some "http" -> LUN.Tcp_client.connect sa
  | Some _ | None -> fail (Failure "unknown scheme")

let connect ?interrupt ?(ssl=false) ~host ~service () =
  lwt sa = LUN.build_sockaddr host service in
  match ssl with
  | true -> LUNS.connect sa
  | false -> LUN.Tcp_client.connect sa

let close_in ic =
  ignore_result (try_lwt Lwt_io.close ic with _ -> return ())

let close_out oc =
  ignore_result (try_lwt Lwt_io.close oc with _ -> return ())

let close' ic oc =
  try_lwt Lwt_io.close oc with _ -> return () >>= fun () ->
  try_lwt Lwt_io.close ic with _ -> return ()

let close ic oc =
  ignore_result (close' ic oc)
