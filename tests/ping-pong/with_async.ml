open Rresult
open Async

let () = Mirage_crypto_rng_unix.initialize ()

module Stop = struct
  type t = unit Deferred.t
end

include Common.Make
          (struct
            include Conduit_async.IO

            let yield () = Async.Deferred.return ()
          end)
          (Stop)
          (Conduit_async)

let tls_protocol, tls_service =
  let open Conduit_async_tls.TCP in
  (protocol, service)

let ctx = Conduit_async.empty
let ctx = Conduit_async.TCP.resolve ctx
let ctx =
  let null ~host:_ _ = Ok None in
  let cfg = Tls.Config.client ~authenticator:null () in
  Conduit_async_tls.TCP.credentials cfg ctx
let localhost = Domain_name.(host_exn (of_string_exn "localhost"))
let ctx = Conduit_async.TCP.domain_name localhost ctx

let failwith fmt = Format.kasprintf (fun err -> raise (Failure err)) fmt

let run_with :
    type cfg service flow.
    ctx:Conduit.context ->
    (cfg, service, flow) Conduit_async.Service.t -> cfg -> string list -> unit =
 fun ~ctx service cfg clients ->
  let stop, signal_stop =
    let open Async.Ivar in
    let v = create () in
    (read v, fill v) in
  let main =
    server ~stop service cfg >>= fun (`Initialized server) ->
    let clients =
      let clients = List.map (client ~resolvers:ctx) clients in
      Async.Deferred.all_unit clients >>| signal_stop in
    Async.Deferred.all_unit [ server; clients ] >>| fun () -> shutdown 0 in
  Async.don't_wait_for main ;
  Core.never_returns (Scheduler.go ())

let run_with_tcp clients =
  let ctx = Conduit_async.TCP.port 5000 ctx in
  run_with ~ctx Conduit_async.TCP.service
    (Conduit_async.TCP.Listen (None, Tcp.Where_to_listen.of_port 5000))
    clients

let load_file filename =
  let open Stdlib in
  let ic = open_in filename in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln ;
  close_in ic ;
  Cstruct.of_bytes rs

let config cert key =
  let cert = load_file cert in
  let key = load_file key in
  match
    (X509.Certificate.decode_pem_multiple cert, X509.Private_key.decode_pem key)
  with
  | Ok certs, Ok (`RSA key) ->
      Tls.Config.server ~certificates:(`Single (certs, key)) ()
  | _ -> Fmt.failwith "Invalid key or certificate"

let run_with_tls cert key clients =
  let ctx = Conduit_async.TCP.port 9000 ctx in
  let ctx = Conduit_async_tls.TCP.resolve ctx in
  run_with ~ctx tls_service
    (Conduit_async.TCP.Listen (None, Tcp.Where_to_listen.of_port 9000), config cert key)
    clients

let () =
  match Array.to_list Stdlib.Sys.argv with
  | _ :: "--with-tls" :: cert :: key :: clients -> run_with_tls cert key clients
  | _ :: clients -> run_with_tcp clients
  | [] -> assert false
