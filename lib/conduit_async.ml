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

#import "conduit_config.mlh"

open Core.Std
open Async.Std

exception Ssl_unsupported [@@deriving sexp]

#if HAVE_ASYNC_SSL
open Async_ssl.Std
#endif

module Ssl = struct
#if HAVE_ASYNC_SSL
  type config = {
    version : Ssl.Version.t option;
    name : string option;
    ca_file : string option;
    ca_path : string option;
    session : Ssl.Session.t option sexp_opaque;
    verify : (Ssl.Connection.t -> bool Deferred.t) option;
  } [@@deriving sexp]

  let verify_certificate connection =
    match Ssl.Connection.peer_certificate connection with
    | None -> return false
    | Some (Error _) -> return false
    | Some (Ok _) -> return true

  let configure ?version ?name ?ca_file ?ca_path ?session ?verify () =
    { version; name; ca_file; ca_path; session; verify}
#else
  type config = unit [@@deriving sexp]

  let verify_certificate _ =
    raise Ssl_unsupported

  let configure ?version ?name ?ca_file ?ca_path ?session ?verify () =
    raise Ssl_unsupported
#endif
end

type +'a io = 'a Deferred.t
type ic = Reader.t
type oc = Writer.t

type addr = [
  | `OpenSSL of string * Ipaddr.t * int
  | `OpenSSL_with_config of string * Ipaddr.t * int * Ssl.config
  | `TCP of Ipaddr.t * int
  | `Unix_domain_socket of string
] [@@deriving sexp]

let connect ?interrupt dst =
  match dst with
  | `TCP (ip, port) -> begin
      Tcp.connect ?interrupt (Tcp.to_host_and_port (Ipaddr.to_string ip) port)
      >>= fun (_, rd, wr) -> return (rd,wr)
  end
  | `OpenSSL (host, ip, port) -> begin
#if HAVE_ASYNC_SSL
      Tcp.connect ?interrupt (Tcp.to_host_and_port (Ipaddr.to_string ip) port)
      >>= fun (_, rd, wr) ->
      Conduit_async_ssl.ssl_connect rd wr
#else
      raise Ssl_unsupported
#endif
  end
  | `OpenSSL_with_config (host, ip, port, config) -> begin
#if HAVE_ASYNC_SSL
      Tcp.connect ?interrupt (Tcp.to_host_and_port (Ipaddr.to_string ip) port)
      >>= fun (_, rd, wr) ->
      let open Ssl in
      match config with | {version; name; ca_file; ca_path; session; verify} ->
      Conduit_async_ssl.ssl_connect ?version ?name ?ca_file ?ca_path ?session ?verify rd wr
#else
      raise Ssl_unsupported
#endif
  end
  | `Unix_domain_socket file -> begin
      Tcp.connect ?interrupt (Tcp.to_file file)
      >>= fun (_, rd, wr) ->
      return (rd,wr)
  end

type trust_chain = [
  | `Ca_file of string
  | `Ca_path of string
  | `Search_file_first_then_path of
      [ `File of string ] *
      [ `Path of string ]
] [@@deriving sexp]

type openssl = [
  | `OpenSSL of
      [ `Crt_file_path of string ] *
      [ `Key_file_path of string ]
] [@@deriving sexp]

type requires_async_ssl = [
  | openssl
  | `OpenSSL_with_trust_chain of openssl * trust_chain
] [@@deriving sexp]

type server = [
  | `TCP
  | requires_async_ssl
] [@@deriving sexp]

let serve
      ?max_connections ?backlog
      ?buffer_age_limit ?on_handler_error mode where_to_listen handle_request =
  let handle_client handle_request sock rd wr =
    match mode with
    | `TCP -> handle_request sock rd wr
    | #requires_async_ssl as async_ssl ->
#if HAVE_ASYNC_SSL
        let (crt_file, key_file, ca_file, ca_path) =
          match async_ssl with
          | `OpenSSL (`Crt_file_path crt_file, `Key_file_path key_file) ->
            (crt_file, key_file, None, None)
          | `OpenSSL_with_trust_chain
              (`OpenSSL (`Crt_file_path crt, `Key_file_path key), trust_chain) ->
            let (ca_file, ca_path) =
              match trust_chain with
              | `Ca_file ca_file -> (Some ca_file, None)
              | `Ca_path ca_path -> (None, Some ca_path)
              | `Search_file_first_then_path (`File ca_file, `Path ca_path) ->
                (Some ca_file, Some ca_path)
            in
            (crt, key, ca_file, ca_path)
        in
        Conduit_async_ssl.ssl_listen ?ca_file ?ca_path ~crt_file ~key_file rd wr
        >>= fun (rd,wr) -> handle_request sock rd wr
#else
        raise Ssl_unsupported
#endif
    in
    Tcp.Server.create ?max_connections ?backlog
      ?buffer_age_limit ?on_handler_error
      where_to_listen (handle_client handle_request)
