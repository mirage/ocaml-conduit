open Rresult
open Async

let () = Mirage_crypto_rng_unix.initialize ()

include Common.Make
          (struct
            type +'a t = 'a Async.Deferred.t

            let bind x f = Async.Deferred.bind x ~f

            let return = Async.Deferred.return

            let yield () = Async.Deferred.return ()
          end)
          (Async.Condition)
          (struct
            type 'a condition = 'a Async.Condition.t

            include Conduit_async
          end)

let tcp_protocol, tcp_service =
  let open Conduit_async.TCP in
  (protocol, service)

let tls_protocol, tls_service =
  let open Conduit_async_tls.TCP in
  (protocol, service)

let failwith fmt = Format.kasprintf (fun err -> raise (Failure err)) fmt

let resolve_ping_pong = Conduit_async.TCP.resolve ~port:5000

let resolve_tls_ping_pong =
  let null ~host:_ _ = Ok None in
  let config = Tls.Config.client ~authenticator:null () in
  Conduit_async_tls.TCP.resolve ~port:9000 ~config

let resolvers =
  Conduit.empty
  |> Conduit_async.add ~priority:10 tls_protocol resolve_tls_ping_pong
  |> Conduit_async.add ~priority:20 tcp_protocol resolve_ping_pong

let localhost = Domain_name.(host_exn (of_string_exn "localhost"))

let run_with :
    type cfg service flow.
    cfg ->
    service:(cfg, service, flow) Conduit_async.Service.service ->
    string list ->
    unit =
 fun cfg ~service clients ->
  let stop, server = server (* ~launched ~stop *) cfg ~service in
  let clients =
    Async.after Core.Time.Span.(of_sec 0.5) >>= fun () ->
    (* XXX(dinosaure): [async] tries to go further and fibers
     * can be launched before the initialization of the server.
     * We waiting a bit to ensure that the server is launched
     * before clients. *)
    let clients = List.map (client ~resolvers) clients in
    Async.Deferred.all_unit clients >>= fun () ->
    Condition.broadcast stop () ;
    Async.return () in
  Async.don't_wait_for
    (Async.Deferred.all_unit [ server (); clients ] >>| fun () -> shutdown 0) ;
  Core.never_returns (Scheduler.go ())

let run_with_tcp clients =
  run_with
    (Conduit_async.TCP.Listen (None, Tcp.Where_to_listen.of_port 5000))
    ~service:tcp_service clients

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
  let ctx = config cert key in
  run_with
    (Conduit_async.TCP.Listen (None, Tcp.Where_to_listen.of_port 9000), ctx)
    ~service:tls_service clients

let () =
  match Array.to_list Stdlib.Sys.argv with
  | _ :: "--with-tls" :: cert :: key :: clients -> run_with_tls cert key clients
  | _ :: clients -> run_with_tcp clients
  | [] -> assert false
