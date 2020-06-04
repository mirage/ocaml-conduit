open Async
include Conduit_tls.Make (Conduit_async.Async_scheduler) (Conduit_async)

module TCP = struct
  open Conduit_async_tcp

  let protocol = protocol_with_tls protocol

  let service = service_with_tls service protocol

  let resolv_conf ~port ~config domain_name =
    resolv_conf ~port domain_name >>| function
    | Some edn -> Some (edn, config)
    | None -> None
end
