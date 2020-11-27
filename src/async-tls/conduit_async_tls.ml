open Async
include Conduit_tls.Make (Conduit_async.IO) (Conduit_async)

module TCP = struct
  open Conduit_async.TCP

  let flow = flow_with_tls flow

  let protocol = protocol_with_tls flow protocol

  let service = service_with_tls flow service

  let resolve ~port ~config domain_name =
    resolve ~port domain_name >>| function
    | Some edn -> Some (edn, config)
    | None -> None
end
