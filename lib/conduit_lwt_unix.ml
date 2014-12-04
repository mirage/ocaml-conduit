(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 Hannes Mehnert <hannes@mehnert.org>
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

type tls_lib = | OpenSSL | Native | No_tls
let tls_library = ref No_tls
let () =
IFDEF HAVE_LWT_SSL THEN
  IFDEF HAVE_LWT_TLS THEN
    tls_library := try
        match Sys.getenv "CONDUIT_TLS" with
        | "native" | "Native" | "NATIVE" -> Native
        | _ -> OpenSSL
      with Not_found -> OpenSSL
  ELSE
    tls_library := OpenSSL
  END
ELSE
  IFDEF HAVE_LWT_TLS THEN
      tls_library := Native
  ELSE
      tls_library := No_tls
  END
END

type +'a io = 'a Lwt.t
type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel

type client = [
  | `TLS of string * Ipaddr.t * int
  | `TCP of Ipaddr.t * int
  | `Unix_domain_socket of string
  | `Vchan_direct of int * string
  | `Vchan_domain_socket of string * string
] with sexp

type server = [
  | `TLS of
      [ `Crt_file_path of string ] *
      [ `Key_file_path of string ] *
      [ `Password of bool -> string | `No_password ] *
      [ `Port of int ]
  | `TCP of [ `Port of int ]
  | `Unix_domain_socket of [ `File of string ]
  | `Vchan_direct of int * string
  | `Vchan_domain_socket of string * string
] with sexp

type tls_server_key = [
  | `None
  | `TLS of
      [ `Crt_file_path of string ] *
      [ `Key_file_path of string ] *
      [ `Password of bool -> string | `No_password ]
] with sexp

type ctx = {
  src: Unix.sockaddr option;
  tls_server_key: tls_server_key;
}

let string_of_unix_sockaddr sa =
  let open Unix in
  match sa with
  | ADDR_UNIX s ->
      Printf.sprintf "ADDR_UNIX(%s)" s
  | ADDR_INET (ia, port) ->
      Printf.sprintf "ADDR_INET(%s,%d)" (string_of_inet_addr ia) port

let sexp_of_ctx ctx =
  <:sexp_of< string option * tls_server_key >>
    ((match ctx.src with |None -> None
      |Some sa -> Some (string_of_unix_sockaddr sa)),
     ctx.tls_server_key)

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
  port: string;
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

let safe_close t =
  Lwt.catch
    (fun () -> Lwt_io.close t)
    (fun _ -> return_unit)

(* Vanilla sockaddr connection *)
module Sockaddr_client = struct
  let connect ?src sa =
    let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sa) Unix.SOCK_STREAM 0 in
    let () =
      match src with
      | None -> ()
      | Some src_sa -> Lwt_unix.bind fd src_sa
    in
    Lwt_unix.connect fd sa >>= fun () ->
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
    return (fd, ic, oc)

  let close (ic,oc) =
    safe_close oc >>= fun () ->
    safe_close ic

end

module Sockaddr_server = struct

  let close (ic, oc) =
    safe_close oc >>= fun () ->
    safe_close ic

  let init_socket sockaddr =
    Unix.handle_unix_error (fun () ->
      let sock = Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
      Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
      Lwt_unix.bind sock sockaddr;
      Lwt_unix.listen sock 15;
      sock) ()

  let process_accept ?timeout callback (client,_) =
    Lwt_unix.setsockopt client Lwt_unix.TCP_NODELAY true;
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input client in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.output client in
    let c = callback client ic oc in
    let events = match timeout with
      |None -> [c]
      |Some t -> [c; (Lwt_unix.sleep (float_of_int t)) ] in
    let _ = Lwt.pick events >>= fun () -> close (ic,oc) in
    return ()

  let init ~sockaddr ?(stop = fst (Lwt.wait ())) ?timeout callback =
    let cont = ref true in
    let s = init_socket sockaddr in
    async (fun () ->
      stop >>= fun () ->
      cont := false;
      return_unit
    );
    let rec loop () =
      if not !cont then return_unit
      else
        Lwt_unix.accept s >>=
        process_accept ?timeout callback >>= fun () ->
        loop ()
    in
    loop ()

end

let connect ~ctx (mode:client) =
  match mode with
  | `TCP (ip,port) ->
    let sa = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port) in
    Sockaddr_client.connect ?src:ctx.src sa
    >>= fun (fd, ic, oc) ->
    let flow = TCP {fd;ip;port} in
    return (flow, ic, oc)
  | `Unix_domain_socket path ->
    Sockaddr_client.connect (Unix.ADDR_UNIX path)
    >>= fun (fd, ic, oc) ->
    let flow = Domain_socket {fd; path} in
    return (flow, ic, oc)
  | `TLS (host, ip, port) ->
    (match !tls_library with
     | OpenSSL ->
IFDEF HAVE_LWT_SSL THEN
       let sa = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip,port) in
       Conduit_lwt_unix_ssl.Client.connect ?src:ctx.src sa
       >>= fun (fd, ic, oc) ->
       let flow = TCP {fd;ip;port} in
       return (flow, ic, oc)
ELSE
       fail (Failure "No SSL support compiled into Conduit")
END
     | Native ->
IFDEF HAVE_LWT_TLS THEN
       let sa = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip,port) in
       Conduit_lwt_tls.Client.connect ?src:ctx.src host sa >|= fun (fd, ic, oc) ->
       let flow = TCP { fd ; ip ; port } in
       (flow, ic, oc)
ELSE
       fail (Failure "No TLS support compiled into Conduit")
END
     | No_tls -> fail (Failure "No SSL or TLS support compiled into Conduit") )
  | `Vchan_direct (domid, sport) ->
IFDEF HAVE_VCHAN_LWT THEN
    begin match Vchan.Port.of_string sport with
      | `Error s -> fail (Failure ("Invalid vchan port: " ^ s))
      | `Ok p -> return p
    end >>= fun port ->
    let flow = Vchan { domid; port=sport } in
    Vchan_lwt_unix.open_client ~domid ~port () >>= fun (ic, oc) ->
    return (flow, ic, oc)
ELSE
    fail (Failure "No Vchan support compiled into Conduit")
END
  | `Vchan_domain_socket uuid ->
    fail (Failure "Vchan_domain_socket not implemented")

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
  | `TCP (`Port port) ->
       let sockaddr, ip = sockaddr_on_tcp_port ctx port in
       Sockaddr_server.init ~sockaddr ?timeout ?stop
         (fun fd ic oc -> callback (TCP {fd; ip; port}) ic oc);
       >>= fun () -> t
  |  `Unix_domain_socket (`File path) ->
       let sockaddr = Unix.ADDR_UNIX path in
       Sockaddr_server.init ~sockaddr ?timeout ?stop
         (fun fd ic oc -> callback (Domain_socket {fd;path}) ic oc);
       >>= fun () -> t
  | `TLS (`Crt_file_path certfile, `Key_file_path keyfile, pass, `Port port) ->
    (match !tls_library with
     | OpenSSL ->
IFDEF HAVE_LWT_SSL THEN
       let sockaddr, ip = sockaddr_on_tcp_port ctx port in
       let password = match pass with
        | `No_password -> None
        | `Password fn -> Some fn
       in
       Conduit_lwt_unix_ssl.Server.init
         ?password ~certfile ~keyfile ?timeout ?stop sockaddr
         (fun fd ic oc -> callback (TCP {fd;ip;port}) ic oc) >>= fun () ->
       t
ELSE
       fail (Failure "No SSL support compiled into Conduit")
END
     | Native ->
IFDEF HAVE_LWT_TLS THEN
       let sockaddr, ip = sockaddr_on_tcp_port ctx port in
       (match pass with
        | `No_password -> return ()
        | `Password _ -> fail (Failure "OCaml-TLS cannot handle encrypted pem files") ) >>= fun () ->
       Conduit_lwt_tls.Server.init
         ~certfile ~keyfile ?timeout ?stop sockaddr
         (fun fd ic oc -> callback (TCP {fd;ip;port}) ic oc) >>= fun () ->
       t
ELSE
       fail (Failure "No TLS support compiled into Conduit")
END
     | No_tls -> fail (Failure "No SSL or TLS support compiled into Conduit") )
  |`Vchan_direct (domid, sport) ->
IFDEF HAVE_VCHAN_LWT THEN
    begin match Vchan.Port.of_string sport with
      | `Error s -> fail (Failure ("Invalid vchan port: " ^ s))
      | `Ok p -> return p
    end >>= fun port ->
    Vchan_lwt_unix.open_server ~domid ~port () >>= fun (ic, oc) ->
    callback (Vchan {domid; port=sport}) ic oc
ELSE
    fail (Failure "No Vchan support compiled into Conduit")
END
  | `Vchan_domain_socket uuid ->
    fail (Failure "Vchan_domain_socket not implemented")

let endp_of_flow = function
  | TCP { ip; port; _ } -> `TCP (ip, port)
  | Domain_socket { path; _ } -> `Unix_domain_socket path
  | Vchan { domid; port } -> `Vchan_direct (domid, port)

(** Use the configuration of the server to interpret how to
    handle a particular endpoint from the resolver into a
    concrete implementation of type [client] *)
let endp_to_client ~ctx (endp:Conduit.endp) =
  match endp with
  | `TCP (_ip, _port) as mode -> return mode
  | `Unix_domain_socket _path as mode -> return mode
  | `Vchan_direct _ as mode -> return mode
  | `Vchan_domain_socket _ as mode -> return mode
  | `TLS (host, (`TCP (ip, port))) -> return (`TLS (host, ip, port))
  | `TLS (host, endp) -> begin
       fail (Failure (Printf.sprintf
         "TLS to non-TCP currently unsupported: host=%s endp=%s"
         host (Sexplib.Sexp.to_string_hum (Conduit.sexp_of_endp endp))))
  end
  | `Unknown err -> fail (Failure ("resolution failed: " ^ err))

let endp_to_server ~ctx (endp:Conduit.endp) =
  match endp with
  | `Unix_domain_socket path -> return (`Unix_domain_socket (`File path))
  | `TLS (_host, `TCP (_ip, port)) -> begin
       match ctx.tls_server_key with
       | `None -> fail (Failure "No TLS server key configured")
       | `TLS (`Crt_file_path crt, `Key_file_path key, pass) ->
          return (`TLS (`Crt_file_path crt, `Key_file_path key,
            pass, `Port port))
     end
  | `TCP (_ip, port) -> return (`TCP (`Port port))
  | `Vchan_direct _ as mode -> return mode
  | `Vchan_domain_socket _ as mode -> return mode
  | `TLS (_host, _) -> fail (Failure "TLS to non-TCP currently unsupported")
  | `Unknown err -> fail (Failure ("resolution failed: " ^ err))
