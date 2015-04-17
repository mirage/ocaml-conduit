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
  *)

module Dynamic_flow: V1_LWT.FLOW
(** An implementation of [V1_LWT.FLOW] using first-class modules and
    existentials. *)

(** {1 VCHAN} *)

(** The signature for Vchan implementations. *)
module type VCHAN = sig
  type t with sexp_of
  type flow with sexp_of
  type uuid = string

  type port with sexp
  (** Type of the port name that identifies a unique connection at
      an endpoint *)

  val port_of_string: string -> [ `Ok of port | `Error of string ]
  (** Convert a string into a Vchan port. *)

  (** Module type of a Vchan endpoint *)
  module Endpoint: sig

    type t
    (** The type for Vchan endpoints. *)

    type error = [`Unknown of string]

    (** [server ~domid ~port ?read_size ?write_size ()] will listen on a
        connection for a source [domid] and [port] combination, block
        until a client connects, and then return a {!t} handle to read
        and write on the resulting connection.  The size of the shared
        memory buffer can be controlled by setting [read_size] or
        [write_size] in bytes. *)
    val server :
      domid:int ->
      port:port ->
      ?read_size:int ->
      ?write_size:int ->
      unit -> t Lwt.t

    (** [client ~domid ~port ()] will connect to a remote [domid] and
        [port] combination, where a server should already be listening
        after making a call to {!server}.  The call will block until a
        connection is established, after which it will return a {!t}
        handle that can be used to read or write on the shared memory
        connection. *)
    val client :
      domid:int ->
      port:port ->
      unit -> t Lwt.t

    include V1_LWT.FLOW
      with type flow = t
       and  type error := error
       and  type 'a io = 'a Lwt.t
       and  type buffer = Cstruct.t

  end

  val register : uuid -> t Lwt.t

  val listen : t -> Conduit.endp Lwt_stream.t Lwt.t

  val connect : t -> remote_name:uuid -> port:port -> Conduit.endp Lwt.t

end

module No_Vchan: VCHAN
(** A dummy implementation for the {!VCHAN} signature. *)

(** {1 TLS} *)
module type TLS = sig

  (** Signature for TLS implementations. *)

  module FLOW : V1_LWT.FLOW with type flow = Dynamic_flow.flow
  (** Underlying (encrypted) flow *)

  include V1_LWT.FLOW

  type client with sexp
  (** The type for TLS client configuration. *)

  type server with sexp
  (** The type for TLS server configuration. *)

  type tracer
  (** The type for tracer configuration. *)

  val server_of_flow : ?trace:tracer -> server -> FLOW.flow ->
    [> `Ok of flow | `Error of error | `Eof  ] Lwt.t
  (** [server_of_flow: c f] is the TLS flow using the server
      configuration [c] and the underlying flow [f] .*)

  val client_of_flow: client -> FLOW.flow ->
    [> `Ok of flow | `Error of error | `Eof] Lwt.t
 (** [client_of_flow c f] is the TLS client flow using the TLS
     client configuration [c] and the underlying flow [f].. *)

end

module No_TLS : TLS
(** A dummy implementation of the {!TLS} signature. *)

(** {1 Main} *)

(** The main Conduit signature. Abstract away TCP, TLS and Vchan
    connections.*)
module type S = sig

  type vchan_port
  (** The type for Vchan ports. *)

  type tls_client
  (** The type for TLS client configurations. *)

  type tls_server
  (** The type for TLS server configuration. *)

  (** Configuration for a single client connection. Can be either of:

      {ul
      {- {b TLS:} client configuration and underlying flow configuration}
      {- {b TCP:} IP address and TCP port number}
      {- {b Vchan direct:} Remote Xen domain id and port name}
      {- {b Vchan domain socket:} {i TODO}}
      }
 *)
  type client = [
    | `TLS of tls_client * client
    | `TCP of Ipaddr.t * int
    | `Vchan_direct of int * vchan_port
    | `Vchan_domain_socket of [ `Uuid of string ] * [ `Port of vchan_port ]
  ] with sexp

  (** Configuration for listening on a server port. Can be either of
      {i FIXME}. *)
  type server = [
    | `TLS of tls_server * server
    | `TCP of [ `Port of int ]
    | `Vchan_direct of [ `Remote_domid of int ] * vchan_port
    | `Vchan_domain_socket of [ `Uuid of string ] * [ `Port of vchan_port ]
  ] with sexp

  module Flow : V1_LWT.FLOW
  type +'a io = 'a Lwt.t
  type ic = Flow.flow
  type oc = Flow.flow
  type flow = Flow.flow
  type stack
  type peer

  type ctx with sexp_of
  val default_ctx : ctx

  val init : ?peer:peer -> ?stack:stack -> unit -> ctx io

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

module Make(S:V1_LWT.STACKV4)(V: VCHAN)(T:TLS) :
  S with type stack = S.t
     and type peer = V.t
     and type tls_client = T.client
     and type tls_server = T.server
     and type vchan_port = V.port
