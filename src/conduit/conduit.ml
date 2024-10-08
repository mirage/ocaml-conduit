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

open Sexplib0.Sexp_conv

type endp =
  [ `TCP of Ipaddr_sexp.t * int  (** ipaddr and dst port *)
  | `Unix_domain_socket of string  (** unix file path *)
  | `Vchan_direct of int * string  (** domain id, port *)
  | `Vchan_domain_socket of string * string
  | `TLS of string * endp  (** wrap in a TLS channel, [hostname,endp] *)
  | `Unknown of string  (** failed resolution *) ]
[@@deriving sexp]
(** The resolver will return an [endp], which the Conduit backend must interpret
    to make a connection. *)

module type IO = sig
  type +'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end
