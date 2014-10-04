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

(** Functorial connection establishment interface that is compatible with
    the Mirage libraries.

    Currently supports two transports: 

    - TCPv4 for remote communications using the {{:https://www.ietf.org/rfc/rfc793.txt}TCPv4} protocol
    - Vchan for inter-VM communication within a single Xen host
  *)

IFDEF HAVE_VCHAN THEN
type vchan_port = Vchan.Port.t with sexp
ELSE
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

(** Module type of a Vchan endpoint *)
module type ENDPOINT = sig

  (** Type of a single connection *)
  type t with sexp_of

  (** Type of the port name that identifies a unique connection at an endpoint *)
  type port = vchan_port

  type error = [
    `Unknown of string
  ]

  (** [server ~domid ~port ?read_size ?write_size] will listen on a connection for
      a source [domid] and [port] combination, block until a client connects, and 
      then return a {!t} handle to read and write on the resulting connection.
      The size of the shared memory buffer can be controlled by setting [read_size]
      or [write_size] in bytes. *)
  val server :
    domid:int ->
    port:port ->
    ?read_size:int ->
    ?write_size:int ->
    unit -> t Lwt.t

  (** [client ~domid ~port] will connect to a remote [domid] and [port] combination,
    where a server should already be listening after making a call to {!server}.
    The call will block until a connection is established, after which it will return
    a {!t} handle that can be used to read or write on the shared memory connection. *)
  val client :
    domid:int ->
    port:port ->
    unit -> t Lwt.t

  (** Close a Vchan. This deallocates the Vchan and attempts to free
      its resources. The other side is notified of the close, but can
      still read any data pending prior to the close. *)
  val close : t -> unit Lwt.t

  include V1_LWT.FLOW
    with type flow = t
    and  type error := error
    and  type 'a io = 'a Lwt.t
    and  type buffer = Cstruct.t
end

(** Functor to construct a {!V1_LWT.FLOW} module that internally contains
    all of the supported transport mechanisms, such as TCPv4 and Vchan. *)
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
