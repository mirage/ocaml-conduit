open Mirage

let net =
  try match Sys.getenv "NET" with
    | "direct" -> `Direct
    | _        -> `Socket
  with Not_found -> `Socket

let dhcp =
  try match Sys.getenv "ADDR" with
    | "dhcp"   -> `Dhcp
    | _ -> `Static
  with Not_found -> `Static

let stack console =
  match net, dhcp with
  | `Direct, `Dhcp   -> direct_stackv4_with_dhcp console tap0
  | `Direct, `Static -> direct_stackv4_with_default_ipv4 console tap0
  | `Socket, _       -> socket_stackv4 console [Ipaddr.V4.any]

let client =
  foreign ~deps:[abstract nocrypto] "Unikernel.Client" @@ console @-> stackv4 @-> job

let () =
  register
    ~libraries:[ "conduit.lwt"; "conduit.mirage"; "vchan" ]
    "http-server"
    [ client $ default_console $ stack default_console ]
