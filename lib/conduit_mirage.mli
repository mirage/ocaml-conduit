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

(** Functorial interface to Conduit that is compatible with the Mirage
    module types.  Currently supports two transports: TCPv4 for remote
    communications, and Vchan for inter-VM communication within a single
    Xen host. *)

IFDEF HAVE_VCHAN THEN
(** A Vchan port name *)
type vchan_port = Vchan.Port.t with sexp
ELSE
(** Vchan is not available in this library: recompile it with 
    the [vchan] package from OPAM to enable support *)
type vchan_port = [ `Vchan_not_available ] with sexp
ENDIF

(** Configuration for a single client connection *)
type client = [
  | `TCP of Ipaddr.t * int     (** IP address and TCP port number *)
  | `Vchan of int * vchan_port (** Remote Xen domain id and port name *)
] with sexp

(** Configuration for listening on a server port. *)
type server = [
  | `TCP of [ `Port of int ]
  | `Vchan of int * vchan_port
] with sexp

module type ENDPOINT = sig
  type t with sexp_of
  type port = vchan_port

  type error = [
    `Unknown of string
  ]

  val server :
    domid:int ->
    port:port ->
    ?read_size:int ->
    ?write_size:int ->
    unit -> t Lwt.t

  val client :
    domid:int ->
    port:port ->
    unit -> t Lwt.t

  val close : t -> unit Lwt.t
  (** Close a vchan. This deallocates the vchan and attempts to free
      its resources. The other side is notified of the close, but can
      still read any data pending prior to the close. *)

  include V1_LWT.FLOW
    with type flow = t
    and  type error := error
    and  type 'a io = 'a Lwt.t
    and  type buffer = Cstruct.t
end

module Make_flow(S:V1_LWT.TCPV4)(V:ENDPOINT) : V1_LWT.FLOW

module type S = sig

  module Flow : V1_LWT.FLOW
  type +'a io = 'a Lwt.t
  type ic = Flow.flow
  type oc = Flow.flow
  type flow = Flow.flow
  type stack

  type ctx
  val default_ctx : ctx

  val init : stack -> ctx io

  val connect : ctx:ctx -> client -> (flow * ic * oc) io

  val serve :
    ?timeout:int -> ?stop:(unit io) -> ctx:ctx ->
     mode:server -> (flow -> ic -> oc -> unit io) -> unit io

  val endp_to_client: ctx:ctx -> Conduit.endp -> client io
  (** Use the configuration of the server to interpret how to handle a
      particular endpoint from the resolver into a concrete
      implementation of type [client] *)

  val endp_to_server: ctx:ctx -> Conduit.endp -> server io
end

module Make(S:V1_LWT.STACKV4)(V: ENDPOINT) : S with type stack = S.t
