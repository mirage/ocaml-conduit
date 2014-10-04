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

(** Interface for establishing reliable stream-oriented connections.

    This library abstracts the concerns of establishing connections to
    peers that may be running within the same host (e.g. in another
    virtual machine) or on a remote host via TCP.  It consists of one
    library that is responsible for {{!transport}establishing individual
    connections}, and a {{!resolution}name resolver} that maps URIs to endpoints.

    {2:transport Connection Establishment} 

    Connections are created by identifying remote nodes using an {{!endp}endp} value.
    To ensure portability, the {!endp} values are translated into concrete
    connections by separate modules that target [Lwt_unix], [Async] and [Mirage].
    This lets those backends use the appropriate local technique for creating the
    connection (such as using OpenSSL on Unix, or a pure OCaml TLS+TCP
    implementation on Mirage, or some other combination).

    The modules dealing with connection establishment are:
    {!modules: Conduit_lwt_unix Conduit_async Conduit_mirage}

    {2:resolution Name Resolution}

    This deals with resolving URIs into a list of {!endp} addresses that can
    then be connected to by the {{!transport}connection establishment} modules.

    All of the name resolvers conform to the {!RESOLVER} module type.
    The OS-specific implementations of this interface are:
    {!modules: Conduit_resolver_lwt Conduit_resolver_lwt_unix Conduit_resolver_mirage}
   *)

(** End points that can potentially be connected to.
    These are typically returned by a call to a {{!resolution}resolver}. *)
type endp = [
  | `TCP of Ipaddr.t * int        (** IP address and destination port *)
  | `Unix_domain_socket of string (** Unix domain file path *)
  | `Vchan of int * string        (** domain id, port *)
  | `TLS of string * endp         (** Wrap in a TLS channel, [hostname,endp] *)
  | `Unknown of string            (** Failed resolution *)
] with sexp

(** Module type for cooperative threading that can be satisfied by
    Lwt or Async *)
module type IO = sig
  type +'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

(** Module type for a {{!resolution}resolver} that can map URIs to
    concrete {{!endp}endpoints} that stream connections can be
    established with. *)
module type RESOLVER = sig

  (** Abstract type of the cooperative threading library used, normally
      defined via the {!IO} module type *)
  type +'a io

  (** State handle for a running resolver *)
  type t with sexp

  (** Abstract type for a service entry, which maps a URI scheme into
      a protocol handler and TCP port *)
  type svc

  (** A rewrite function resolves a {{!svc}service} and a URI into
      a concrete endpoint. *)
  type rewrite_fn = svc -> Uri.t -> endp io

  (** A service function maps the string (such as [http] or [ftp]) from
      a URI scheme into a {{!svc}service} description that includes
      enough metadata about the service to subsequently {{!rewrite_fn}resolve}
      it into an {{!endp}endpoint}. *)
  type service_fn = string -> svc option io

  (** [init ?service ?rewrites] will initialize the resolver and return
      a state handler.  The {{!service_fn}service} argument should
      contain the system-specific resolution mechanism for URI schemas.

      The [rewrites] argument can optionally override a subset of the
      URI domain name with the given {!rewrite_fn} to permit custom
      resolution rules.  For example, a rewrite rule for ".xen" would
      let the rewrite function resolve hostnames such as "foo.xen"
      into a shared memory channel for the "foo" virtual machine. *)
  val init :
    ?service:service_fn -> ?rewrites:(string * rewrite_fn) list ->
    unit -> t

  (** [add_rewrite ~host f t] will add to the [t] resolver the [f] rewrite rule
      for all the domain names that shortest-prefix match [host] *)
  val add_rewrite : host:string -> f:rewrite_fn -> t -> unit

  (** [resolve_uri ?rewrites ~uri t] will use [t] to resolve the
      [uri] into a concrete endpoint.  Any [rewrites] that are passed
      in will be overlayed on the existing rules within the [t]
      resolver, but not otherwise modify it. *)
  val resolve_uri :
    ?rewrites:(string * rewrite_fn) list ->
    uri:Uri.t -> t -> endp io
end
