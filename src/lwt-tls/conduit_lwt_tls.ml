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

  let protocol = protocol_with_tls protocol

  include (val Conduit_lwt.repr protocol)

  let service = service_with_tls service Conduit_lwt.TCP.protocol protocol

  let resolve ~port ~config domain_name =
    let open Lwt.Infix in
    resolve ~port domain_name >|= function
    | Some edn -> Some (edn, config)
    | None -> None

  let configuration ~config:tls_config ?capacity sockaddr =
    (configuration ?capacity sockaddr, tls_config)
end
