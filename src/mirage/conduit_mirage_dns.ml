open Lwt.Infix

module Make
    (R : Mirage_random.S)
    (T : Mirage_time.S)
    (C : Mirage_clock.MCLOCK)
    (S : Mirage_stack.V4) (V : sig
      val endpoint :
        (S.t, Ipaddr.V4.t) Conduit_mirage_tcp.endpoint Conduit_mirage.value
    end) =
struct
  include Dns_client_mirage.Make (R) (T) (C) (S)

  let port : int Conduit_mirage.value = Conduit_mirage.info ~name:"port"

  let domain_name : [ `host ] Domain_name.t Conduit_mirage.value =
    Conduit_mirage.info ~name:"domain-name"

  let keep_alive : Mirage_protocols.Keepalive.t Conduit_mirage.value =
    Conduit_mirage.info ~name:"keep-alive"

  let no_delay : bool Conduit_mirage.value =
    Conduit_mirage.info ~name:"no-delay"

  let resolve :
      S.t ->
      t ->
      ?nameserver:Transport.ns_addr ->
      Conduit.context ->
      Conduit.context =
   fun stack t ?nameserver ctx ->
    let gethostbyname domain_name port keepalive nodelay =
      gethostbyname ?nameserver t domain_name >>= function
      | Ok ip ->
          Lwt.return_some
            { Conduit_mirage_tcp.stack; keepalive; nodelay; ip; port }
      | _ -> Lwt.return_none in
    Conduit_mirage.fold V.endpoint
      Conduit_mirage.Fun.
        [
          req $ domain_name;
          req $ port;
          opt $ keep_alive;
          dft $ (false, no_delay);
        ]
      ~f:gethostbyname ctx

  let port v ctx = Conduit_mirage.add port v ctx

  let domain_name v ctx = Conduit_mirage.add domain_name v ctx

  let keep_alive v ctx = Conduit_mirage.add keep_alive v ctx

  let no_delay v ctx = Conduit_mirage.add no_delay v ctx
end
