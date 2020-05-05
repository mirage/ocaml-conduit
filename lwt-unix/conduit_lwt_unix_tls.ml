include Conduit_tls.Make (Conduit_lwt.Lwt_scheduler) (Conduit_lwt)

module TCP = struct
  open Conduit_lwt_unix_tcp

  let endpoint, protocol = protocol_with_tls ~key:endpoint protocol

  let configuration, service =
    service_with_tls ~key:configuration service protocol

  let resolv_conf ~port ~config domain_name =
    let open Lwt.Infix in
    resolv_conf ~port domain_name >|= function
    | Some edn -> Some (edn, config)
    | None -> None
end
