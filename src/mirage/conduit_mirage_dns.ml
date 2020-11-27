open Lwt.Infix
open Conduit_mirage

module Make
    (R : Mirage_random.S)
    (T : Mirage_time.S)
    (C : Mirage_clock.MCLOCK)
    (S : Mirage_stack.V4) =
struct
  include Dns_client_mirage.Make (R) (T) (C) (S)

  let resolv :
      S.t ->
      ?keepalive:Mirage_protocols.Keepalive.t ->
      ?nodelay:bool ->
      t ->
      ?nameserver:Transport.ns_addr ->
      port:int ->
      (S.t, Ipaddr.V4.t) Conduit_mirage_tcp.endpoint resolver =
   fun stack ?keepalive ?(nodelay = false) t ?nameserver ~port -> function
    | Endpoint.IP (Ipaddr.V6 _) -> Lwt.return_none
    | IP (Ipaddr.V4 ip) ->
        Lwt.return_some
          { Conduit_mirage_tcp.stack; keepalive; nodelay; ip; port }
    | Domain domain_name -> (
        gethostbyname ?nameserver t domain_name >>= function
        | Ok ip ->
            Lwt.return_some
              { Conduit_mirage_tcp.stack; keepalive; nodelay; ip; port }
        | Error _err -> Lwt.return_none)
end
