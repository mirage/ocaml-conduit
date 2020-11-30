open Async
include Conduit_tls.Make (Conduit_async.IO) (Conduit_async)

module TCP = struct
  open Conduit_async.TCP

  let protocol = protocol_with_tls protocol

  let service = service_with_tls service protocol

  let configuration ~config:tls_config ?backlog listen =
    (configuration ?backlog listen, tls_config)

  let resolve ~port ~config domain_name =
    resolve ~port domain_name >>| function
    | Some edn -> Some (edn, config)
    | None -> None
end
