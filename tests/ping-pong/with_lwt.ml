open Rresult

let () = Printexc.record_backtrace true

let failwith fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

module Lwt = struct
  include Lwt

  let yield = Lwt_unix.yield
end

include Common.Make (Lwt) (Lwt_condition)
          (struct
            type 'a condition = 'a Lwt_condition.t

            include Conduit_lwt
          end)

(* Composition *)

let tls_protocol, tls_service =
  let open Conduit_lwt_tls.TCP in
  (protocol, service)

(* Resolution *)

let resolve_ping_pong = Conduit_lwt.TCP.resolve ~port:4000

let resolve_tls_ping_pong =
  let null ~host:_ _ = Ok None in
  let config = Tls.Config.client ~authenticator:null () in
  Conduit_lwt_tls.TCP.resolve ~port:8000 ~config

let resolvers =
  let open Conduit_lwt in
  empty
  |> add ~priority:20 TCP.protocol resolve_ping_pong
  |> add ~priority:10 tls_protocol resolve_tls_ping_pong

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
    (cfg, s, flow) Conduit_lwt.Service.t -> cfg -> string list -> unit =
 fun service cfg clients ->
  let stop, server = server service cfg in
  let clients = List.map (client ~resolvers) clients in
  let clients =
    Lwt.join clients >>= fun () ->
    Lwt_condition.broadcast stop () ;
    Lwt.return_unit in
  Lwt_main.run (Lwt.join [ server (); clients ])

let run_with_tcp clients =
  run_with Conduit_lwt.TCP.service
    {
      Conduit_lwt.TCP.sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 4000);
      capacity = 40;
    }
    clients

let run_with_tls cert key clients =
  let ctx = config cert key in
  run_with tls_service
    ( {
        Conduit_lwt.TCP.sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, 8000);
        capacity = 40;
      },
      ctx )
    clients

let () =
  match Array.to_list Sys.argv with
  | _ :: "--with-tls" :: cert :: key :: clients -> run_with_tls cert key clients
  | _ :: clients -> run_with_tcp clients
  | _ -> Fmt.epr "%s [--with-tls] filename...\n%!" Sys.argv.(0)
