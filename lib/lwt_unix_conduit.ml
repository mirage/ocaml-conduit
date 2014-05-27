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
  src: Unix.sockaddr
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

module Client = struct

  type t = [
    | `SSL of string * int
    | `TCP of string * int
    | `Domain_socket of string
  ] with sexp

  let connect ctx (mode:t) =
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
    | `Domain_socket file ->
       Lwt_unix_net.Sockaddr_client.connect (Unix.ADDR_UNIX file)
end

module Server = struct

  type t = [
    | `SSL of 
       [ `Crt_file_path of string ] * 
       [ `Key_file_path of string ] *
       [ `Password of bool -> string | `No_password ] *
       [ `Port of int]
    | `TCP of [ `Port of int ]
  ] with sexp

  let sockaddr_on_tcp_port ctx port =
    match ctx.src with
    | Unix.ADDR_UNIX _ -> fail (Failure "Cant listen to TCP on a domain socket")
    | Unix.ADDR_INET (a,_) -> return (Unix.ADDR_INET (a,port))

  let serve ?timeout ctx (mode:t) callback =
    match mode with
    | `TCP (`Port port) ->
       lwt sockaddr = sockaddr_on_tcp_port ctx port in
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

let close_in ic =
  ignore_result (try_lwt Lwt_io.close ic with _ -> return ())

let close_out oc =
  ignore_result (try_lwt Lwt_io.close oc with _ -> return ())

let close' ic oc =
  try_lwt Lwt_io.close oc with _ -> return () >>= fun () ->
    try_lwt Lwt_io.close ic with _ -> return ()

let close ic oc =
  ignore_result (close' ic oc)
