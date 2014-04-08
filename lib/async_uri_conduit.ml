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

open Core.Std
open Async.Std

IFDEF HAVE_ASYNC_SSL THEN
open Async_ssl.Std
END

type +'a io = 'a Deferred.t
type ic = Reader.t
type oc = Writer.t

let connect_uri ?interrupt uri =
  let host = Option.value (Uri.host uri) ~default:"localhost" in
  match Uri_services.tcp_port_of_uri ~default:"http" uri with
  | None -> raise (Failure "Net.connect") (* TODO proper exception *)
  | Some port ->
    Tcp.connect ?interrupt (Tcp.to_host_and_port host port)
    >>= fun (_, net_to_ssl, ssl_to_net) ->
IFDEF HAVE_ASYNC_SSL THEN
    match Uri.scheme uri with
    | Some "https" -> Async_net_ssl.ssl_connect net_to_ssl ssl_to_net
    | _ -> return (net_to_ssl, ssl_to_net)
ELSE
    match Uri.scheme uri with
    | Some "https" -> raise (Failure "SSL unsupported")
    | _    -> return (net_to_ssl, ssl_to_net)
END

let connect ?interrupt ?(ssl=false) ~host ~service () =
  match Uri_services.tcp_port_of_service service with
  | [] -> raise (Failure ("Unknown URI scheme " ^ service))
  | port :: _ -> begin
     Tcp.connect ?interrupt (Tcp.to_host_and_port host port)
     >>= fun (_, rd, wr) ->
IFDEF HAVE_ASYNC_SSL THEN
     match ssl with
     | true -> Async_net_ssl.ssl_connect rd wr
     | false -> return (rd,wr)
ELSE
     match ssl with
     | true -> raise (Failure "SSL unsupported")
     | false -> return (rd,wr)
END
  end

let close_in ic = don't_wait_for (Reader.close ic)
let close_out oc = don't_wait_for (Writer.close oc)
let close ic oc =
  close_in ic;
  close_out oc
