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

  let endpoint, protocol = protocol_with_tls protocol

  let service = service_with_tls service Conduit_async.TCP.protocol protocol

  let configuration ~config:tls_config ?backlog listen =
    (configuration ?backlog listen, tls_config)

  let resolve ctx =
    let ctx = resolve ctx in
    Conduit_async.fold endpoint
      Conduit_async.Fun.[ req $ Conduit_async.TCP.endpoint; req $ credentials ]
      ~f:(fun edn cfg -> Async.return (Some (edn, cfg)))
      ctx

  let credentials cfg ctx = Conduit_async.add credentials cfg ctx

  let endpoint edn cfg ctx = Conduit_async.add endpoint (edn, cfg) ctx
end
