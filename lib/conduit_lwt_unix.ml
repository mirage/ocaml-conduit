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
open Sexplib.Conv

type +'a io = 'a Lwt.t
type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel

type client = [
  | `OpenSSL of string * Ipaddr.t * int
  | `TCP of Ipaddr.t * int
  | `Unix_domain_socket of string
  | `Vchan of int * Vchan.Port.t
] with sexp

type server = [
  | `OpenSSL of
      [ `Crt_file_path of string ] *
      [ `Key_file_path of string ] *
      [ `Password of bool -> string | `No_password ] *
      [ `Port of int ]
  | `TCP of [ `Port of int ]
  | `Unix_domain_socket of [ `File of string ]
  | `Vchan of int * Vchan.Port.t
] with sexp

type tls_server_key = [
  | `None
  | `OpenSSL of
      [ `Crt_file_path of string ] *
      [ `Key_file_path of string ] *
      [ `Password of bool -> string | `No_password ]
]

type ctx = {
  src: Unix.sockaddr option;
  tls_server_key: tls_server_key;
}

type tcp_flow = {
  fd: Lwt_unix.file_descr sexp_opaque;
  ip: Ipaddr.t;
  port: int;
} with sexp

type domain_flow = {
  fd: Lwt_unix.file_descr sexp_opaque;
  path: string;
} with sexp

type vchan_flow = {
  domid: int;
  port: Vchan.Port.t;
} with sexp

type flow =
  | TCP of tcp_flow
  | Domain_socket of domain_flow
  | Vchan of vchan_flow
with sexp

let default_ctx =
  { src=None; tls_server_key=`None }

let init ?src ?(tls_server_key=`None) () =
  let open Unix in
  match src with
  | None ->
     return { src=None; tls_server_key }
  | Some host ->
     Lwt_unix.getaddrinfo host "0" [AI_PASSIVE; AI_SOCKTYPE SOCK_STREAM]
     >>= function
     | {ai_addr;_}::_ -> return { src=Some ai_addr; tls_server_key }
     | [] -> fail (Failure "Invalid conduit source address specified")

let connect ~ctx (mode:client) =
  match mode with
  | `OpenSSL (_host, ip, port) ->
IFDEF HAVE_LWT_SSL THEN
      let sa = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip,port) in
      lwt fd, ic, oc = Conduit_lwt_unix_net_ssl.Client.connect ?src:ctx.src sa in
      let flow = TCP {fd;ip;port} in
      return (flow, ic, oc)
ELSE
      fail (Failure "No SSL support compiled into Conduit")
END
  | `TCP (ip,port) ->
       let sa = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port) in
       lwt fd,ic,oc = Conduit_lwt_unix_net.Sockaddr_client.connect ?src:ctx.src sa in
       let flow = TCP {fd;ip;port} in
       return (flow, ic, oc)
  | `Unix_domain_socket path ->
       lwt (fd,ic,oc) = Conduit_lwt_unix_net.Sockaddr_client.connect (Unix.ADDR_UNIX path) in
       let flow = Domain_socket {fd; path} in
       return (flow, ic, oc)
  | `Vchan (domid, port) ->
       let flow = Vchan { domid; port } in
       Vchan_lwt_unix.open_client ~domid ~port () >>= fun (ic, oc) ->
       return (flow, ic, oc)

let sockaddr_on_tcp_port ctx port =
  let open Unix in
  match ctx.src with
  | Some (ADDR_UNIX _) -> raise (Failure "Cant listen to TCP on a domain socket")
  | Some (ADDR_INET (a,_)) -> ADDR_INET (a,port), Ipaddr_unix.of_inet_addr a
  | None -> ADDR_INET (inet_addr_any,port), Ipaddr.(V4 V4.any)

let serve ?timeout ?stop ~(ctx:ctx) ~(mode:server) callback =
  let t, _u = Lwt.task () in (* End this via Lwt.cancel *)
  Lwt.on_cancel t (fun () -> print_endline "Terminating server thread");
  match mode with
    |`Vchan (domid, port) ->
      Vchan_lwt_unix.open_server ~domid ~port () >>= fun (ic, oc) ->
      callback (Vchan {domid; port}) ic oc
  | `TCP (`Port port) ->
       let sockaddr, ip = sockaddr_on_tcp_port ctx port in
       Conduit_lwt_unix_net.Sockaddr_server.init ~sockaddr ?timeout ?stop
         (fun fd ic oc -> callback (TCP {fd; ip; port}) ic oc);
       >>= fun () -> t
  |  `Unix_domain_socket (`File path) ->
       let sockaddr = Unix.ADDR_UNIX path in
       Conduit_lwt_unix_net.Sockaddr_server.init ~sockaddr ?timeout ?stop
         (fun fd ic oc -> callback (Domain_socket {fd;path}) ic oc);
       >>= fun () -> t
  | `OpenSSL (`Crt_file_path certfile, `Key_file_path keyfile, pass, `Port port) ->
IFDEF HAVE_LWT_SSL THEN
       let sockaddr, ip = sockaddr_on_tcp_port ctx port in
       let password = match pass with |`No_password -> None |`Password fn -> Some fn in
       Conduit_lwt_unix_net_ssl.Server.init ?password ~certfile ~keyfile ?timeout ?stop sockaddr
         (fun fd ic oc -> callback (TCP {fd;ip;port}) ic oc);
       >>= fun () -> t
ELSE
       fail (Failure "No SSL support compiled into Conduit")
END

type endp = [
  | `TCP of Ipaddr.t * int        (** IP address and destination port *)
  | `Unix_domain_socket of string (** Unix domain file path *)
  | `Vchan of int * Vchan.Port.t  (** domain id * port *)
  | `TLS of string * endp         (** Wrap in a TLS channel, [hostname,endp] *)
  | `Unknown of string            (** Failed resolution *)
] with sexp

let endp_to_client ~ctx:_ (endp:Conduit.endp) =
  match endp with
  | `TCP (_ip, _port) as mode -> return mode
  | `Unix_domain_socket _path as mode -> return mode
  | `TLS (host, `TCP (ip, port)) -> return (`OpenSSL (host, ip, port))
  | `Vchan (_, _) as mode -> return mode
  | `TLS (_host, _) -> fail (Failure "TLS to non-TCP currently unsupported")
  | `Unknown err -> fail (Failure ("resolution failed: " ^ err))

let endp_to_server ~ctx (endp:Conduit.endp) =
  match endp with
  | `Unix_domain_socket path -> return (`Unix_domain_socket (`File path))
  | `TLS (_host, `TCP (_ip, port)) -> begin
       match ctx.tls_server_key with
       | `None -> fail (Failure "No TLS server key configured")
       | `OpenSSL (`Crt_file_path crt, `Key_file_path key, pass) ->
          return (`OpenSSL (`Crt_file_path crt, `Key_file_path key,
            pass, `Port port))
     end
  | `TCP (_ip, port) -> return (`TCP (`Port port))
  | `Vchan (_, _) as mode -> return mode
  | `TLS (_host, _) -> fail (Failure "TLS to non-TCP currently unsupported")
  | `Unknown err -> fail (Failure ("resolution failed: " ^ err))
