(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Lwt
open Sexplib.Std

type +'a io = 'a Lwt.t
type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel

type client = [
  | `OpenSSL of string * Ipaddr.t * int
  | `TCP of Ipaddr.t * int
  | `Unix_domain_socket of string
] with sexp

type server = [
  | `OpenSSL of
      [ `Crt_file_path of string ] * 
      [ `Key_file_path of string ] *
      [ `Password of bool -> string | `No_password ] *
      [ `Port of int ]
  | `TCP of [ `Port of int ]
  | `Unix_domain_socket of [ `File of string ]
] with sexp

type ctx = {
  src: Unix.sockaddr;
} 

type flow = [
  | `TCP of Unix.file_descr
]

let default_ctx =
  { src=Unix.(ADDR_INET (inet_addr_loopback,0)) }

let init ?src () =
  let open Unix in
  match src with
  | None ->
     return { src=(ADDR_INET (inet_addr_any, 0)) }
  | Some host ->
     Lwt_unix.getaddrinfo host "0" [AI_PASSIVE; AI_SOCKTYPE SOCK_STREAM]
     >>= function
     | [] -> fail (Failure "Invalid conduit source address specified")
     | {ai_addr;_}::_ -> return { src=ai_addr }

let connect ~ctx (mode:client) =
  match mode with
  | `OpenSSL (_host, ip, port) -> 
IFDEF HAVE_LWT_SSL THEN
      let sa = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip,port) in
      Conduit_lwt_unix_net_ssl.Client.connect ~src:ctx.src sa
ELSE
      fail (Failure "No SSL support compiled into Conduit")
END
  | `TCP (ip,port) ->
       let sa = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip,port) in
       Conduit_lwt_unix_net.Sockaddr_client.connect ~src:ctx.src sa
  | `Unix_domain_socket file ->
       Conduit_lwt_unix_net.Sockaddr_client.connect (Unix.ADDR_UNIX file)

let sockaddr_on_tcp_port ctx port =
  match ctx.src with
  | Unix.ADDR_UNIX _ -> fail (Failure "Cant listen to TCP on a domain socket")
  | Unix.ADDR_INET (a,_) -> return (Unix.ADDR_INET (a,port))

let serve ?timeout ?stop ~ctx ~mode callback =
  match mode with
  | `TCP (`Port port) ->
       lwt sockaddr = sockaddr_on_tcp_port ctx port in
       Conduit_lwt_unix_net.Sockaddr_server.init ~sockaddr ?timeout ?stop callback
  | `Unix_domain_socket (`File file) ->
       let sockaddr = Unix.ADDR_UNIX file in
       Conduit_lwt_unix_net.Sockaddr_server.init ~sockaddr ?timeout ?stop callback
  | `OpenSSL (`Crt_file_path certfile, `Key_file_path keyfile, pass, `Port port) -> 
IFDEF HAVE_LWT_SSL THEN
       lwt sockaddr = sockaddr_on_tcp_port ctx port in
       let password = match pass with |`No_password -> None |`Password fn -> Some fn in
       Conduit_lwt_unix_net_ssl.Server.init ?password ~certfile ~keyfile ?timeout ?stop sockaddr callback
ELSE
       fail (Failure "No SSL support compiled into Conduit")
END

(*
module Resolver = struct

  type ctx = {
    src: Unix.sockaddr;
    resolver: Lwt_conduit_resolver.t;
  }

  let init ?src ?(resolver=Lwt_unix_resolver.system) () =
    let open Unix in
    match src with
    | None ->
      return { src=(ADDR_INET (inet_addr_any, 0)); resolver }
    | Some host ->
      Lwt_unix.getaddrinfo host "0" [AI_PASSIVE; AI_SOCKTYPE SOCK_STREAM]
      >>= function
      | [] -> fail (Failure "Invalid conduit source address specified")
      | {ai_addr;_}::_ -> return { src=ai_addr; resolver }

  let system =
    { src=Unix.(ADDR_INET (inet_addr_any, 0));
      resolver=Lwt_unix_resolver.system }

  let default_ctx =
    let open Unix in
    { src = ADDR_INET (inet_addr_any, 0); 
      resolver = Lwt_unix_resolver.system }

  type endp = Lwt_unix.sockaddr

  let peername conn =
    match conn with
    | `TCP fd -> Unix.getpeername fd

  let sockname conn =
    match conn with
    | `TCP fd -> Unix.getsockname fd

end

let connect_to_uri ?(ctx=default_ctx) uri =
  Lwt_conduit_resolver.resolve_uri ~uri ctx.resolver
  >>= function
  | `TCP (_ip,_port) as mode -> connect ~ctx mode
  | `Unix_domain_socket _path as mode -> connect ~ctx mode
  | `TLS (host, `TCP (ip, port)) -> connect ~ctx (`OpenSSL (host, ip, port))
  | `TLS (_host, _) -> fail (Failure "TLS to non-TCP unsupported")
  | `Vchan _path -> fail (Failure "VChan not supported")
  | `Unknown err -> fail (Failure ("resolution failed: " ^ err))
*)


