(*
 * Copyright (c) 2012-2017 Anil Madhavapeddy <anil@recoil.org>
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

open Async

module Ssl = Conduit_async_ssl.Ssl_config

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
      Tcp.connect ?interrupt (Tcp.Where_to_connect.of_host_and_port { host = Ipaddr.to_string ip; port })
      >>= fun (_, rd, wr) -> return (rd,wr)
  end
  | `OpenSSL (_, ip, port) -> begin
      Tcp.connect ?interrupt (Tcp.Where_to_connect.of_host_and_port { host = Ipaddr.to_string ip; port })
      >>= fun (_, rd, wr) ->
      let config = Conduit_async_ssl.Ssl_config.configure () in
      Conduit_async_ssl.ssl_connect config rd wr
  end
  | `OpenSSL_with_config (_, ip, port, config) -> begin
      Tcp.connect ?interrupt (Tcp.Where_to_connect.of_host_and_port { host = Ipaddr.to_string ip; port })
      >>= fun (_, rd, wr) ->
      Conduit_async_ssl.ssl_connect config rd wr
  end
  | `Unix_domain_socket file -> begin
      Tcp.connect ?interrupt (Tcp.Where_to_connect.of_file file)
      >>= fun (_, rd, wr) ->
      return (rd,wr)
  end

let with_connection ?interrupt dst f =
  match dst with
  | `TCP (ip, port) -> begin
      Tcp.with_connection ?interrupt
        (Tcp.Where_to_connect.of_host_and_port { host = Ipaddr.to_string ip; port })
        (fun _ rd wr -> f rd wr)
    end
  | `OpenSSL (_, ip, port) -> begin
    let config = Conduit_async_ssl.Ssl_config.configure () in
    Tcp.with_connection ?interrupt
    (Tcp.Where_to_connect.of_host_and_port { host = Ipaddr.to_string ip; port }) begin fun _ rd wr ->
    Conduit_async_ssl.ssl_connect config rd wr >>= fun (rd, wr) ->
    Monitor.protect (fun () -> f rd wr) ~finally:begin fun () ->
      Deferred.all_unit [ Reader.close rd ; Writer.close wr ]
      end
    end
  end
  | `OpenSSL_with_config (_, ip, port, config) -> begin
    Tcp.with_connection ?interrupt
    (Tcp.Where_to_connect.of_host_and_port { host = Ipaddr.to_string ip; port }) begin fun _ rd wr ->
     Conduit_async_ssl.ssl_connect config rd wr >>= fun (rd, wr) ->
     Monitor.protect (fun () -> f rd wr) ~finally:begin fun () ->
       Deferred.all_unit [ Reader.close rd ; Writer.close wr ]
     end
    end
  end
  | `Unix_domain_socket file -> begin
    Tcp.with_connection ?interrupt (Tcp.Where_to_connect.of_file file)
      (fun _ rd wr -> f rd wr)
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
      ?buffer_age_limit ?(on_handler_error=`Raise) mode where_to_listen handle_request =
  let handle_client handle_request sock rd wr =
    match mode with
    | `TCP -> handle_request sock rd wr
    | #requires_async_ssl as async_ssl ->
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
        Conduit_async_ssl.ssl_listen
          ?ca_file ?ca_path ~crt_file ~key_file rd wr >>= fun (rd,wr) ->
        Monitor.protect
          (fun () -> handle_request sock rd wr)
          ~finally:(fun () ->
              Deferred.all_unit [ Reader.close rd ; Writer.close wr ])
    in
    Tcp.Server.create ?max_connections ?backlog
      ?buffer_age_limit ~on_handler_error
      where_to_listen (handle_client handle_request)
