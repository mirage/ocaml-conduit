open Rresult

let () = Mirage_crypto_rng_unix.initialize ()

let () = Printexc.record_backtrace true

let () = Ssl.init ()

let failwith fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

include Common.Make (Lwt) (Lwt_condition)
          (struct
            type 'a condition = 'a Lwt_condition.t

            include Conduit_lwt
          end)

(* Composition *)

let tls_protocol, tls_service =
  let open Conduit_lwt_unix_tls.TCP in
  (protocol, service)

let ssl_protocol, ssl_service =
  let open Conduit_lwt_unix_ssl.TCP in
  (protocol, service)

(* Resolution *)

let resolve_ping_pong = Conduit_lwt_unix_tcp.resolv_conf ~port:4000

let resolve_tls_ping_pong =
  let null ~host:_ _ = Ok None in
  let config = Tls.Config.client ~authenticator:null () in
  Conduit_lwt_unix_tls.TCP.resolv_conf ~port:8000 ~config

let resolve_ssl_ping_pong =
  let context = Ssl.create_context Ssl.TLSv1_2 Ssl.Client_context in
  Conduit_lwt_unix_ssl.TCP.resolv_conf ~port:6000 ~context ?verify:None

let resolvers =
  Conduit.empty
  |> Conduit_lwt.add ~priority:20 Conduit_lwt_unix_tcp.protocol
       resolve_ping_pong
  |> Conduit_lwt.add ~priority:10 tls_protocol resolve_tls_ping_pong
  |> Conduit_lwt.add ~priority:10 ssl_protocol resolve_ssl_ping_pong

(* Run *)

let load_file filename =
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

let run_with :
    type cfg master flow.
    cfg ->
    protocol:(_, flow) Conduit_lwt.protocol ->
    service:(cfg, master, flow) Conduit_lwt.Service.service ->
    string list ->
    unit =
 fun cfg ~protocol ~service clients ->
  let stop, server = server cfg ~protocol ~service in
  let clients = List.map (client ~resolvers) clients in
  let clients =
    Lwt.join clients >>= fun () ->
    Lwt_condition.broadcast stop () ;
    Lwt.return_unit in
  Lwt_main.run (Lwt.join [ server; clients ])

let run_with_tcp clients =
  run_with
    {
      Conduit_lwt_unix_tcp.sockaddr =
        Unix.ADDR_INET (Unix.inet_addr_loopback, 4000);
      capacity = 40;
    }
    ~protocol:Conduit_lwt_unix_tcp.protocol
    ~service:Conduit_lwt_unix_tcp.service clients

let run_with_ssl cert key clients =
  let ctx = Ssl.create_context Ssl.TLSv1_2 Ssl.Server_context in
  Ssl.use_certificate ctx cert key ;
  run_with
    ( ctx,
      {
        Conduit_lwt_unix_tcp.sockaddr =
          Unix.ADDR_INET (Unix.inet_addr_loopback, 6000);
        capacity = 40;
      } )
    ~protocol:ssl_protocol ~service:ssl_service clients

let run_with_tls cert key clients =
  let ctx = config cert key in
  run_with
    ( {
        Conduit_lwt_unix_tcp.sockaddr =
          Unix.ADDR_INET (Unix.inet_addr_loopback, 8000);
        capacity = 40;
      },
      ctx )
    ~protocol:tls_protocol ~service:tls_service clients

let () =
  match Array.to_list Sys.argv with
  | _ :: "--with-tls" :: cert :: key :: clients -> run_with_tls cert key clients
  | _ :: "--with-ssl" :: cert :: key :: clients -> run_with_ssl cert key clients
  | _ :: clients -> run_with_tcp clients
  | _ -> Fmt.epr "%s [--with-tls|--with-ssl] filename...\n%!" Sys.argv.(0)
