(** MirageOS-functor to be able to resolve a domain-name such as [gethostbyname]
    with [ocaml-dns]. *)

module Make
    (R : Mirage_random.S)
    (T : Mirage_time.S)
    (C : Mirage_clock.MCLOCK)
    (S : Mirage_stack.V4) : sig
  include module type of Dns_client_mirage.Make (R) (T) (C) (S)

  val resolv :
    S.t ->
    ?keepalive:Mirage_protocols.Keepalive.t ->
    t ->
    ?nameserver:Transport.ns_addr ->
    port:int ->
    (S.t, Ipaddr.V4.t) Conduit_mirage_tcp.endpoint Conduit_mirage.resolver
end
