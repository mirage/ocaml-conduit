open Lwt.Infix

module Make
    (R : Mirage_random.S)
    (T : Mirage_time.S)
    (C : Mirage_clock.MCLOCK)
    (S : Mirage_stack.V4V6) =
struct
  include Dns_client_mirage.Make (R) (T) (C) (S)

  let resolv :
      S.t ->
      ?keepalive:Mirage_protocols.Keepalive.t ->
      ?nodelay:bool ->
      t ->
      ?nameserver:Transport.ns_addr ->
      port:int ->
      (S.t, Ipaddr.t) Conduit_mirage_tcp.endpoint Conduit_mirage.resolver =
   fun stack ?keepalive ?(nodelay = false) t ?nameserver ~port -> function
    | IP ip ->
        Lwt.return_some
          { Conduit_mirage_tcp.stack; keepalive; nodelay; ip; port }
    | Domain domain_name -> (
        (* TODO use gethostbyname6 as well (if stack has v6 connectivity),
           implement happy eyeballs *)
        gethostbyname ?nameserver t domain_name
        >>= function
        | Ok ip ->
            let ip = Ipaddr.V4 ip in
            Lwt.return_some
              { Conduit_mirage_tcp.stack; keepalive; nodelay; ip; port }
        | Error _err -> Lwt.return_none)
end
