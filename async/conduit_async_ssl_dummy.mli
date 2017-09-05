(*
 * Copyright (c) 2012-2017 Anil Madhavapeddy <anil@recoil.org>
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

(** TLS/SSL connection establishment using OpenSSL and Async *)
open Async

module Ssl_config : sig
  type config

  val verify_certificate :
    [`Ssl_not_compiled_in] ->
    bool Deferred.t

  val configure :
    ?version:[`Ssl_not_compiled_in] ->
    ?name:string ->
    ?ca_file:string ->
    ?ca_path:string ->
    ?session:[`Ssl_not_compiled_in] ->
    ?verify:([`Ssl_not_compiled_in] -> bool Deferred.t) ->
    unit ->
    config
end

(** [ssl_connect rd wr] will establish a client TLS/SSL session
    over an existing pair of a [rd] {!Reader.t} and [wd] {!Writer.t}
    Async connections. *)
val ssl_connect : Ssl_config.config -> Reader.t -> Writer.t ->
  (Reader.t * Writer.t) Deferred.t

(** [ssl_listen ~crt_file ~key_file rd wr] will establish a server
    TLS/SSL session over an existing pair of [rd] {!Reader.t} and
    [wd] {!Writer.t} Async connections.

    [version] is the version of SSL being used by the server. If not
    set, it is [Ssl.Version.Tlsv1_2].

    From [Async_ssl.Std.Ssl]: If both [ca_file] and [ca_path] are specified,
    the certificates in [ca_file] will be searched before the certificates in
    [ca_path].*)
val ssl_listen :
  ?version:[`Ssl_not_compiled_in] ->
  ?ca_file:string ->
  ?ca_path:string ->
  crt_file:string ->
  key_file:string ->
  Reader.t ->
  Writer.t ->
  (Reader.t * Writer.t) Deferred.t
