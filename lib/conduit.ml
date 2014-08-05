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

open Sexplib.Std

(** The resolver will return an [endp], which the Conduit
    backend must interpret to make a connection. *)
type endp = [
  | `TCP of Ipaddr.t * int        (** ipaddr and dst port *)
  | `Unix_domain_socket of string (** unix file path *)
  | `Vchan of string list         (** xenstore path *)
  | `TLS of string * endp         (** wrap in a TLS channel, [hostname,endp] *)
  | `Unknown of string            (** failed resolution *)
] with sexp

module Client = struct

  type t = [
    | `OpenSSL of string * Ipaddr.t * int
    | `TCP of Ipaddr.t * int
    | `Unix_domain_socket of string
  ] with sexp

end

module Server = struct

  type t = [
    | `Lwt_ssl of
       [ `Crt_file_path of string ] * 
       [ `Key_file_path of string ] *
       [ `Password of bool -> string | `No_password ] *
       [ `Port of int ]
    | `TCP of [ `Port of int ]
    | `Unix_domain_socket of [ `File of string ]
  ] with sexp

end

module type IO = sig
  type +'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type RESOLVER = sig
  type +'a io
  type t with sexp
  type svc

  type rewrite_fn = svc -> Uri.t -> endp io
  type service_fn = string -> svc option io

  val init :
    ?service:service_fn -> ?rewrites:(string * rewrite_fn) list ->
    unit -> t

  val add_rewrite : host:string -> f:rewrite_fn -> t -> unit

  val resolve_uri :
    ?rewrites:(string * rewrite_fn) list ->
    uri:Uri.t -> t -> endp io
end

