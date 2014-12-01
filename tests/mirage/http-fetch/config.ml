open Mirage

let net =
  try match Sys.getenv "NET" with
    | "direct" -> `Direct
    | _        -> `Socket
  with Not_found -> `Direct

let dhcp =
  try match Sys.getenv "ADDR" with
    | "dhcp"   -> `Dhcp
    | "static" -> `Static
  with Not_found -> `Dhcp

let stack console =
  match net, dhcp with
  | `Direct, `Dhcp   -> direct_stackv4_with_dhcp console tap0
  | `Direct, `Static -> direct_stackv4_with_default_ipv4 console tap0
  | `Socket, _       -> socket_stackv4 console [Ipaddr.V4.any]

let client =
  foreign "Unikernel.Client" @@ console @-> stackv4 @-> job

let () =
  add_to_ocamlfind_libraries [ "conduit.lwt"; "conduit.mirage"; "dns.mirage" ];
  add_to_opam_packages [ "mirage-dns"; "conduit" ];
  register "conduit-client" [ client $ default_console $ stack default_console ]
