open Async

module IO = struct
  type +'a t = 'a Async.Deferred.t

  let bind x f = Async.Deferred.bind x ~f

  let return = Async.Deferred.return

  let catch f ferr =
    Async.try_with ~extract_exn:true f >>= function
    | Ok v -> return v
    | Error exn -> ferr exn

  let fail exn = raise exn
end

include Conduit_tls.Make (IO) (Conduit_async)

module TCP = struct
  open Conduit_async.TCP

  let protocol = protocol_with_tls protocol

  let service ?listening_on () =
    service_with_tls (service ?listening_on ()) Conduit_async.TCP.protocol protocol

  let configuration ~config:tls_config ?backlog listen =
    (configuration ?backlog listen, tls_config)

  let resolve ~port ~config domain_name =
    resolve ~port domain_name >>| function
    | Some edn -> Some (edn, config)
    | None -> None
end
