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
val static : (string, (port:int -> Conduit.endp)) Hashtbl.t -> Conduit_resolver_lwt.t

(** [localhost] is a static resolver that has a single entry that
    maps [localhost] to [127.0.0.1], and fails on all other hostnames. *)
val localhost : Conduit_resolver_lwt.t

(** Given a DNS resolver {{:https://github.com/mirage/ocaml-dns}implementation},
    provide a {!Conduit_resolver_lwt} that can perform DNS lookups to return
    endpoints. *)
module Make(DNS:Dns_resolver_mirage.S) : sig

  (** Default resolver to use, which is [8.8.8.8] (Google DNS). *)
  val default_ns : Ipaddr.V4.t

  (** [system ?ns ?dns_port stack] will return a resolver that uses
      the stub resolver [ns] on port [dns_port] to resolve URIs via
      the [stack] network interface. *)
  val system :
    ?ns:Ipaddr.V4.t -> ?dns_port:int ->
    DNS.stack -> Conduit_resolver_lwt.t
end

module type PEER = sig
  type t
  type flow
  type uuid
  type port

  val register : uuid -> t Lwt.t
  val accept : t -> flow Lwt.t
  val connect : t -> remote_name:uuid -> port:port -> flow Lwt.t
  val exists : t -> uuid -> bool Lwt.t
end
