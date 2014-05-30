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

type ctx = {
  src: Unix.sockaddr;
}

let init ?src () =
   let open Unix in
   match src with
   | None -> return { src=(ADDR_INET (inet_addr_any, 0)) }
   | Some host ->
      Lwt_unix.getaddrinfo host "0" [AI_PASSIVE; AI_SOCKTYPE SOCK_STREAM]
      >>= function
      | [] -> fail (Failure "Invalid conduit source address specified")
      | {ai_addr;_}::_ -> return { src=ai_addr }

let default_ctx =
  let open Unix in
  { src = ADDR_INET (inet_addr_any, 0) }

type conn = [
  | `TCP of Unix.file_descr
]

type endp = Lwt_unix.sockaddr

let peername conn =
  match conn with
  | `TCP fd -> Unix.getpeername fd

let sockname conn =
  match conn with
  | `TCP fd -> Unix.getsockname fd

module Client = struct

  let connect ?(ctx=default_ctx) (mode:Conduit.Client.t) =
    match mode with
    | `SSL (host,port) -> 
IFDEF HAVE_LWT_SSL THEN
      lwt sa = Lwt_unix_net.build_sockaddr host (string_of_int port) in
      Lwt_unix_net_ssl.Client.connect ~src:ctx.src sa
ELSE
      fail (Failure "No SSL support compiled into Conduit")
END
    | `TCP (host,port) ->
       lwt sa = Lwt_unix_net.build_sockaddr host (string_of_int port) in
       Lwt_unix_net.Sockaddr_client.connect ~src:ctx.src sa
    | `Unix_domain_socket file ->
       Lwt_unix_net.Sockaddr_client.connect (Unix.ADDR_UNIX file)
end

module Server = struct

  let sockaddr_on_tcp_port ctx port =
    match ctx.src with
    | Unix.ADDR_UNIX _ -> fail (Failure "Cant listen to TCP on a domain socket")
    | Unix.ADDR_INET (a,_) -> return (Unix.ADDR_INET (a,port))

  let serve ?timeout ?(ctx=default_ctx) (mode:Conduit.Server.t) callback =
    match mode with
    | `TCP (`Port port) ->
       lwt sockaddr = sockaddr_on_tcp_port ctx port in
       Lwt_unix_net.Sockaddr_server.init ~sockaddr ?timeout callback
    | `Unix_domain_socket (`File file) ->
       let sockaddr = Unix.ADDR_UNIX file in
       Lwt_unix_net.Sockaddr_server.init ~sockaddr ?timeout callback
    | `SSL (`Crt_file_path certfile, `Key_file_path keyfile, pass, `Port port) -> 
IFDEF HAVE_LWT_SSL THEN
       lwt sockaddr = sockaddr_on_tcp_port ctx port in
       let password = match pass with |`No_password -> None |`Password fn -> Some fn in
       Lwt_unix_net_ssl.Server.init ?password ~certfile ~keyfile ?timeout sockaddr callback
ELSE
       fail (Failure "No SSL support compiled into Conduit")
END
end
