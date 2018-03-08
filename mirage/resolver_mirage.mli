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

(** Functorial interface for resolving URIs to endpoints. *)

(** [static hosts] constructs a resolver that looks up any resolution
    requests from the static [hosts] hashtable instead of using the
    system resolver. *)
val static : (string, (port:int -> Conduit.endp)) Hashtbl.t -> Conduit_lwt.Resolver.t

(** [localhost] is a static resolver that has a single entry that
    maps [localhost] to [127.0.0.1], and fails on all other hostnames. *)
val localhost : Conduit_lwt.Resolver.t

(** Module allowing to build a {!Conduit_lwt.Resolver} than can perform DNS lookups. *)
module type S = sig
  module DNS : Dns_resolver_mirage.S

  (** Default resolver to use, which is [8.8.8.8] (Google DNS). *)
  val default_ns : Ipaddr.V4.t

  val vchan_resolver : tld:string -> Conduit_lwt.Resolver.rewrite_fn

  (** [dns_stub_resolver ?ns ?dns_port dns] will return a resolver that uses
      the stub resolver [ns] on port [ns_port] to resolve URIs via
      the [dns] network interface. *)
  val dns_stub_resolver:
    ?ns:Ipaddr.V4.t -> ?ns_port:int -> DNS.t -> Conduit_lwt.Resolver.rewrite_fn

  (** [register ?ns ?ns_port ?stack res] TODO *)
  val register:
    ?ns:Ipaddr.V4.t -> ?ns_port:int -> ?stack:DNS.stack ->
    Conduit_lwt.Resolver.t -> unit

  (** [init ?ns ?ns_port ?stack ()] TODO *)
  val init:
    ?ns:Ipaddr.V4.t -> ?ns_port:int -> ?stack:DNS.stack -> unit -> Conduit_lwt.Resolver.t
end

(** Given a DNS resolver {{:https://github.com/mirage/ocaml-dns}implementation},
    provide a {!Conduit_lwt.Resolver} that can perform DNS lookups to return
    endpoints. *)
module Make(DNS:Dns_resolver_mirage.S) : S with module DNS = DNS

(** Provides a DNS-enabled {!Conduit_lwt.Resolver} given a network stack.
    See {!Make}.
*)
module Make_with_stack (T: Mirage_types_lwt.TIME) (S: Mirage_types_lwt.STACKV4) : sig
  include Conduit_lwt.Resolver.S with type t = Conduit_lwt.Resolver.t
  module R : S with type DNS.stack = S.t
end
