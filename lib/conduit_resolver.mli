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

(** The resolver will return an [endp], which the Conduit
    backend must interpret to make a connection. *)
type endp = [
  | `TCP of Ipaddr.t * int        (** ipaddr and dst port *)
  | `Unix_domain_socket of string (** unix file path *)
  | `Vchan of string list         (** xenstore path *)
  | `TLS of string * endp         (** wrap in a TLS channel, [hostname,endp] *)
  | `Unknown of string            (** failed resolution *)
] with sexp

type service = {
  name: string;
  port: int;
  tls: bool
} with sexp

type t with sexp

type fn = service -> Uri.t -> endp Lwt.t

val init :
  ?service:(string -> service option Lwt.t) ->
  ?rewrites:(string * fn) list ->
  unit -> t

val add_domain_rewrite :
  host:string ->
  f:fn -> t -> unit

val resolve_uri :
  ?rewrites:(string * fn) list -> 
  uri:Uri.t -> 
  t ->
  endp Lwt.t
