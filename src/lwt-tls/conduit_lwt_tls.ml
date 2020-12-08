module IO = struct
  type +'a t = 'a Lwt.t

  let bind = Lwt.bind

  let return = Lwt.return

  let fail = Lwt.fail

  let catch = Lwt.catch
end

include Conduit_tls.Make (IO) (Conduit_lwt)

let () = Mirage_crypto_rng_lwt.initialize ()

module TCP = struct
  open Conduit_lwt.TCP

  let endpoint, protocol = protocol_with_tls protocol

  include (val Conduit_lwt.repr protocol)

  let service = service_with_tls service Conduit_lwt.TCP.protocol protocol

  let configuration ~config:tls_config ?capacity sockaddr =
    (configuration ?capacity sockaddr, tls_config)

  let resolve ctx =
    let ctx = resolve ctx in
    Conduit_lwt.fold endpoint
      Conduit_lwt.Fun.[ req $ Conduit_lwt.TCP.endpoint; req $ credentials ]
      ~f:(fun edn cfg -> Lwt.return (Some (edn, cfg)))
      ctx

  let credentials cfg ctx = Conduit_lwt.add credentials cfg ctx

  let endpoint edn cfg ctx = Conduit_lwt.add endpoint (edn, cfg) ctx
end
