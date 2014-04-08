(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 Clark Gaebel
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
open Async_ssl.Std

let ssl_connect net_to_ssl ssl_to_net =
  let net_to_ssl = Reader.pipe net_to_ssl in
  let ssl_to_net = Writer.pipe ssl_to_net in
  let app_to_ssl, app_wr = Pipe.create () in
  let app_rd, ssl_to_app = Pipe.create () in
  don't_wait_for (Ssl.client ~app_to_ssl ~ssl_to_app ~net_to_ssl ~ssl_to_net ());
  Reader.of_pipe (Info.of_string "cohttp_client_reader") app_rd >>= fun app_rd ->
  Writer.of_pipe (Info.of_string "cohttp_client_writer") app_wr >>| fun (app_wr,_) ->
  app_rd, app_wr

let ssl_listen ~crt_file ~key_file rd wr =
  let net_to_ssl = Reader.pipe rd in
  let ssl_to_net = Writer.pipe wr in
  let app_to_ssl, app_wr = Pipe.create () in
  let app_rd, ssl_to_app = Pipe.create () in
  Ssl.server
    ~crt_file
    ~key_file
    ~app_to_ssl
    ~ssl_to_app
    ~net_to_ssl
    ~ssl_to_net
  () |> don't_wait_for;
  Reader.of_pipe (Info.of_string "cohttp_server_reader") app_rd >>= fun app_rd ->
  Writer.of_pipe (Info.of_string "cohttp_server_writer") app_wr >>| fun (app_wr,_) ->
  app_rd, app_wr
