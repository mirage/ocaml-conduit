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

type service = {
  name: string;
  port: int;
  tls: bool
} with sexp

type t with sexp

type rewrite_fn = service -> Uri.t -> Conduit.endp Lwt.t
type service_fn = string -> service option Lwt.t

val init :
  ?service:service_fn ->
  ?rewrites:(string * rewrite_fn) list ->
  unit -> t

val add_rewrite :
  host:string ->
  f:rewrite_fn -> t -> unit

val resolve_uri :
  ?rewrites:(string * rewrite_fn) list -> 
  uri:Uri.t -> 
  t -> Conduit.endp Lwt.t
