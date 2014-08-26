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

type addr = [
  | `OpenSSL of string * Ipaddr.t * int
  | `TCP of Ipaddr.t * int
  | `Unix_domain_socket of string
] with sexp

let connect ?interrupt dst =
  match dst with
  | `TCP (ip, port) -> begin
      Tcp.connect ?interrupt (Tcp.to_host_and_port (Ipaddr.to_string ip) port)
      >>= fun (_, rd, wr) -> return (rd,wr)
  end
  | `OpenSSL (host, ip, port) -> begin
IFDEF HAVE_ASYNC_SSL THEN
      Tcp.connect ?interrupt (Tcp.to_host_and_port (Ipaddr.to_string ip) port)
      >>= fun (_, rd, wr) ->
      Conduit_async_net_ssl.ssl_connect rd wr
ELSE
      raise (Failure "SSL unsupported")
END
  end
  | `Unix_domain_socket file -> begin
      Tcp.connect ?interrupt (Tcp.to_file file)
      >>= fun (_, rd, wr) ->
      return (rd,wr)
  end

type server = [
  | `OpenSSL of
    [ `Crt_file_path of string ] * 
    [ `Key_file_path of string ] 
  | `TCP
] with sexp

let serve
      ?max_connections ?max_pending_connections
      ?buffer_age_limit ?on_handler_error mode where_to_listen handle_request =
  let handle_client handle_request sock rd wr =
    match mode with
    | `TCP -> handle_request sock rd wr
    | `OpenSSL (`Crt_file_path crt_file, `Key_file_path key_file) ->
IFDEF HAVE_ASYNC_SSL THEN
        Conduit_async_net_ssl.ssl_listen ~crt_file ~key_file rd wr
        >>= fun (rd,wr) -> handle_request sock rd wr
ELSE
        raise (Failure "SSL unsupported in Conduit")
END
    in
    Tcp.Server.create ?max_connections ?max_pending_connections
      ?buffer_age_limit ?on_handler_error
      where_to_listen (handle_client handle_request)
