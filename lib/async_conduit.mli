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

type +'a io = 'a Deferred.t
type ic = Reader.t
type oc = Writer.t

module Client : sig
  val connect : ?interrupt:unit io -> Conduit.Client.t -> (ic * oc) io
end

module Server : sig
  type mode = [
    | `OpenSSL of
       [ `Crt_file_path of string ] * 
       [ `Key_file_path of string ]
    | `TCP
  ] with sexp

  val create :
    ?max_connections:int ->
    ?max_pending_connections:int ->
    ?buffer_age_limit:Writer.buffer_age_limit ->
    ?on_handler_error:[ `Call of ([< Socket.Address.t ] as 'a) -> exn -> unit
                      | `Ignore
                      | `Raise ] ->
    mode ->
    ('a, 'b) Tcp.Where_to_listen.t ->
    ('a -> ic -> oc -> unit io) -> 
    ('a, 'b) Tcp.Server.t io
end
