open Conduit_mirage
open Lwt.Infix

module Make
    (R : Mirage_random.S)
    (T : Mirage_time.S)
    (C : Mirage_clock.MCLOCK)
    (S : Mirage_stack.V4) = struct
  include Dns_client_mirage.Make (R) (T) (C) (S)

  let resolv : t -> ?nameserver:Transport.ns_addr -> port:int -> (Ipaddr.V4.t * int) resolver =
    fun t ?nameserver ~port domain_name -> gethostbyname ?nameserver t domain_name >>= function
      | Ok domain_name -> Lwt.return_some (domain_name, port)
      | Error _err -> Lwt.return_none
end
