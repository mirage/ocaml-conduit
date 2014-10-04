(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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

(** Resolve URIs to endpoints *)

(** Description of a single service.
    Can be populated from [/etc/services] with the exception of the
    [tls] field, which indicates if the connection is intended to be
    TLS/SSL-encrypted or not (e.g. for [https]).  *)
type service = {
  name: string;
  port: int;
  tls: bool
} with sexp

(** Functor to construct a concrete resolver using a {!Conduit.IO}
    implementation, usually via either Lwt or Async *)
module Make (IO : Conduit.IO) : Conduit.RESOLVER
  with type svc = service
  and  type 'a io = 'a IO.t

