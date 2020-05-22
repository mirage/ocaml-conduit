open Conduit_mirage

module Make
    (R : Mirage_random.S)
    (T : Mirage_time.S)
    (C : Mirage_clock.MCLOCK)
    (S : Mirage_stack.V4) : sig
  include module type of Dns_client_mirage.Make (R) (T) (C) (S)

  val resolv : t -> ?nameserver:Transport.ns_addr -> port:int -> (Ipaddr.V4.t * int) resolver
end
