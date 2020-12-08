(** MirageOS-functor to be able to resolve a domain-name such as [gethostbyname]
    with [ocaml-dns]. *)

module Make
    (R : Mirage_random.S)
    (T : Mirage_time.S)
    (C : Mirage_clock.MCLOCK)
    (S : Mirage_stack.V4) (V : sig
      val endpoint :
        (S.t, Ipaddr.V4.t) Conduit_mirage_tcp.endpoint Conduit_mirage.value
    end) : sig
  include module type of Dns_client_mirage.Make (R) (T) (C) (S)

  val resolve :
    S.t ->
    t ->
    ?nameserver:Transport.ns_addr ->
    Conduit.context ->
    Conduit.context

  val port : int -> Conduit.context -> Conduit.context

  val domain_name :
    [ `host ] Domain_name.t -> Conduit.context -> Conduit.context

  val keep_alive :
    Mirage_protocols.Keepalive.t -> Conduit.context -> Conduit.context

  val no_delay : bool -> Conduit.context -> Conduit.context
end
