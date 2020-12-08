open Rresult

let () = Printexc.record_backtrace true

let failwith fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

module Lwt = struct
  include Lwt
  include Conduit_lwt.IO

  let yield = Lwt_unix.yield
end

include Common.Make (Lwt) (Lwt_switch) (Conduit_lwt)

(* Composition *)

let tls_protocol, tls_service =
  let open Conduit_lwt_tls.TCP in
  (protocol, service)

(* Resolution *)

let ctx = Conduit_lwt.empty

let ctx = Conduit_lwt.TCP.resolve ctx

let ctx =
  let null ~host:_ _ = Ok None in
  let cfg = Tls.Config.client ~authenticator:null () in
  Conduit_lwt_tls.TCP.credentials cfg ctx

let localhost = Domain_name.(host_exn (of_string_exn "localhost"))

let ctx = Conduit_lwt.TCP.domain_name localhost ctx

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
    type cfg s flow.
    ctx:Conduit.context ->
    (cfg, s, flow) Conduit_lwt.Service.t ->
    cfg ->
    string list ->
    unit =
 fun ~ctx service cfg clients ->
  let stop = Lwt_switch.create () in
  let main =
    server ~stop service cfg >>= fun (`Initialized server) ->
    let clients = List.map (client ~resolvers:ctx) clients in
    let clients = Lwt.join clients >>= fun () -> Lwt_switch.turn_off stop in
    Lwt.join [ server; clients ] in
  Lwt_main.run main

let run_with_tcp clients =
  let ctx = Conduit_lwt.TCP.port 4000 ctx in
  run_with ~ctx Conduit_lwt.TCP.service
    {
      Conduit_lwt.TCP.sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 4000);
      capacity = 40;
    }
    clients

let run_with_tls cert key clients =
  let ctx = Conduit_lwt.TCP.port 8000 ctx in
  let ctx = Conduit_lwt_tls.TCP.resolve ctx in
  run_with ~ctx tls_service
    ( {
        Conduit_lwt.TCP.sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8000);
        capacity = 40;
      },
      config cert key )
    clients

let () =
  match Array.to_list Sys.argv with
  | _ :: "--with-tls" :: cert :: key :: clients -> run_with_tls cert key clients
  | _ :: clients -> run_with_tcp clients
  | _ -> Fmt.epr "%s [--with-tls] filename...\n%!" Sys.argv.(0)
