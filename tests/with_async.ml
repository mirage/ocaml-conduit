open Rresult
open Async
open Async_ssl

let () = Mirage_crypto_rng_unix.initialize ()

let tcp_protocol, tcp_service =
  let open Conduit_async_tcp in
  (protocol, service)

let ssl_protocol, ssl_service =
  let open Conduit_async_ssl.TCP in
  (protocol, service)

let tls_protocol, tls_service =
  let open Conduit_async_tls.TCP in
  (protocol, service)

let ( >>? ) x f =
  x >>= function Ok x -> f x | Error _ as err -> Async.return err

let failwith fmt = Format.kasprintf (fun err -> raise (Failure err)) fmt

let getline queue =
  let exists ~predicate queue =
    let pos = ref 0 and res = ref (-1) in
    Ke.Rke.iter
      (fun chr ->
        if predicate chr then res := !pos ;
        incr pos)
      queue ;
    if !res = -1 then None else Some !res in
  let blit src src_off dst dst_off len =
    Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len in
  match exists ~predicate:(( = ) '\n') queue with
  | Some pos ->
      let tmp = Bytes.create pos in
      Ke.Rke.N.keep_exn queue ~blit ~length:Bytes.length ~off:0 ~len:pos tmp ;
      Ke.Rke.N.shift_exn queue (pos + 1) ;
      Some (Bytes.unsafe_to_string tmp)
  | None -> None

let getline queue flow =
  let tmp = Cstruct.create 0x1000 in
  let blit src src_off dst dst_off len =
    let src = Cstruct.to_bigarray src in
    Bigstringaf.blit src ~src_off dst ~dst_off ~len in
  let rec go () =
    match getline queue with
    | Some line -> Async.return (Ok (`Line line))
    | None -> (
        Conduit_async.Client.recv flow tmp >>? function
        | `End_of_flow -> Async.return (Ok `Close)
        | `Input len ->
            Ke.Rke.N.push queue ~blit ~length:Cstruct.len ~off:0 ~len tmp ;
            go ()) in
  go ()

let pong = Cstruct.of_string "pong\n"

let ping = Cstruct.of_string "ping\n"

let transmission ~stop flow =
  let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.Char in
  let rec go () =
    let finish = Condition.wait stop >>= fun () -> Async.return (Ok `Done) in
    let getline = getline queue flow in
    Async.Deferred.any [ finish; getline ] >>= function
    | Ok (`Done | `Close) | Error _ -> Conduit_async.Client.close flow
    | Ok (`Line "ping") ->
        Format.eprintf "[!] received ping.\n%!" ;
        Conduit_async.Client.send flow pong >>? fun _ -> go ()
    | Ok (`Line "pong") ->
        Format.eprintf "[!] received pong.\n%!" ;
        Conduit_async.Client.send flow ping >>? fun _ -> go ()
    | Ok (`Line line) ->
        Format.eprintf "[!] received %S.\n%!" line ;
        Conduit_async.Client.send flow (Cstruct.of_string (line ^ "\n"))
        >>? fun _ -> Conduit_async.Client.close flow in
  go () >>= function
  | Error err -> failwith "%a" Conduit_async.Client.pp_error err
  | Ok () -> Async.return ()

let server :
    type cfg master flow.
    launched:unit Async.Condition.t ->
    stop:unit Async.Condition.t ->
    cfg ->
    protocol:(_, flow) Conduit_async.Client.protocol ->
    service:(cfg, master, flow) Conduit_async.Service.service ->
    unit Async.Deferred.t =
 fun ~launched ~stop cfg ~protocol ~service ->
  let module Server = (val Conduit_async.Service.impl service) in
  let main () =
    let reword_error = R.reword_error (R.msgf "%a" Server.pp_error) in
    Conduit_async.Service.serve cfg ~service >>? fun master ->
    Condition.signal launched () ;

    let rec go () =
      let close = Async.Condition.wait stop >>| fun () -> Ok `Closed in
      let accept =
        Server.accept master >>? fun flow ->
        Async.(Deferred.ok (return (`Flow flow))) in

      Async.Deferred.any [ close; accept ] >>= function
      | Ok (`Flow flow) ->
          Async.don't_wait_for
            (transmission ~stop (Conduit_async.Client.abstract protocol flow)) ;
          Async.Scheduler.yield () >>= go
      | Ok `Closed -> Server.close master
      | Error _ as err -> Server.close master >>= fun _ -> Async.return err
    in
    go () >>| reword_error in
  main () >>= function
  | Ok () -> Async.return ()
  | Error err -> failwith "%a" Conduit_async.Service.pp_error err

let client ~resolvers domain_name responses =
  Conduit_async.Client.connect resolvers domain_name >>? fun flow ->
  let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
  let rec go = function
    | [] -> Conduit_async.Client.close flow
    | line :: rest -> (
        Conduit_async.Client.send flow (Cstruct.of_string (line ^ "\n"))
        >>? fun _ ->
        getline queue flow >>? function
        | `Close -> Conduit_async.Client.close flow
        | `Line "pong" -> go rest
        | `Line _ -> Conduit_async.Client.close flow) in
  go responses

let client ~resolvers domain_name filename =
  let rec go acc ic =
    match Stdlib.input_line ic with
    | line -> go (line :: acc) ic
    | exception End_of_file -> List.rev acc in
  let ic = Stdlib.open_in filename in
  let responses = go [] ic in
  Stdlib.close_in ic ;
  client ~resolvers domain_name responses >>= function
  | Ok () -> Async.return ()
  | Error (#Conduit_async.Client.error as err) ->
      failwith "Client got an error: %a" Conduit_async.Client.pp_error err

let resolve_ping_pong = Conduit_async_tcp.resolv_conf ~port:5000

let resolve_ssl_ping_pong =
  let context =
    Conduit_async_ssl.context ~verify_modes:Ssl.Verify_mode.[ Verify_none ] ()
  in
  Conduit_async_ssl.TCP.resolv_conf ~port:7000 ~context

let resolve_tls_ping_pong =
  let null ~host:_ _ = Ok None in
  let config = Tls.Config.client ~authenticator:null () in
  Conduit_async_tls.TCP.resolv_conf ~port:9000 ~config

let resolvers =
  Conduit.empty
  |> Conduit_async.Client.add ~priority:10 ssl_protocol resolve_ssl_ping_pong
  |> Conduit_async.Client.add ~priority:10 tls_protocol resolve_tls_ping_pong
  |> Conduit_async.Client.add ~priority:20 tcp_protocol resolve_ping_pong

let localhost = Domain_name.(host_exn (of_string_exn "localhost"))

let run_with :
    type cfg master flow.
    cfg ->
    protocol:(_, flow) Conduit_async.Client.protocol ->
    service:(cfg, master, flow) Conduit_async.Service.service ->
    string list ->
    unit =
 fun cfg ~protocol ~service clients ->
  let launched = Condition.create () in
  let stop = Condition.create () in
  let server () = server ~launched ~stop cfg ~protocol ~service in
  let clients =
    Condition.wait launched >>= fun () ->
    let clients = List.map (client ~resolvers localhost) clients in
    Async.Deferred.all_unit clients >>= fun () ->
    Condition.broadcast stop () ;
    Async.return () in
  Async.don't_wait_for
    (Async.Deferred.all_unit [ server (); clients ] >>| fun () -> shutdown 0) ;
  Core.never_returns (Scheduler.go ())

let run_with_tcp clients =
  run_with
    (Conduit_async_tcp.Listen (Tcp.Where_to_listen.of_port 5000))
    ~protocol:tcp_protocol ~service:tcp_service clients

let run_with_ssl cert key clients =
  let ctx = Conduit_async_ssl.context ~crt_file:cert ~key_file:key () in
  run_with
    (ctx, Conduit_async_tcp.Listen (Tcp.Where_to_listen.of_port 7000))
    ~protocol:ssl_protocol ~service:ssl_service clients

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
    (Conduit_async_tcp.Listen (Tcp.Where_to_listen.of_port 9000), ctx)
    ~protocol:tls_protocol ~service:tls_service clients

let () =
  match Array.to_list Stdlib.Sys.argv with
  | _ :: "--with-ssl" :: cert :: key :: clients -> run_with_ssl cert key clients
  | _ :: "--with-tls" :: cert :: key :: clients -> run_with_tls cert key clients
  | _ :: clients -> run_with_tcp clients
  | [] -> assert false
