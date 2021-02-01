open Lwt.Infix

let client : Conduit_mirage.client =
  `TCP (Ipaddr.of_string_exn "127.0.0.1", 12345)

let server : Conduit_mirage.server = `TCP 12345

module TCP = Conduit_mirage.TCP (Tcpip_stack_socket.V4)

let tcp () =
  Udpv4_socket.connect Ipaddr.V4.Prefix.global >>= fun udp ->
  Tcpv4_socket.connect Ipaddr.V4.Prefix.global >>= fun tcp ->
  Tcpip_stack_socket.V4.connect udp tcp

let _client () = tcp () >>= fun t -> TCP.connect t client

let _server () =
  tcp () >>= fun t -> TCP.listen t server (fun _flow -> Lwt.return ())
