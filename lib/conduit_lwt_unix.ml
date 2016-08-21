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

#import "conduit_config.mlh"

open Lwt
open Sexplib.Conv

let debug = ref false
let debug_print = ref Printf.eprintf
let () =
  try
    ignore(Sys.getenv "CONDUIT_DEBUG");
    debug := true
  with Not_found -> ()

type tls_lib = | OpenSSL | Native | No_tls [@@deriving sexp]
let tls_library = ref No_tls
let () =
#if HAVE_LWT_SSL
#if HAVE_LWT_TLS
    tls_library := try
        match Sys.getenv "CONDUIT_TLS" with
        | "native" | "Native" | "NATIVE" -> Native
        | _ -> OpenSSL
      with Not_found -> OpenSSL
#else
    tls_library := OpenSSL
#endif
#else
#if HAVE_LWT_TLS
      tls_library := Native
#else
      tls_library := No_tls
#endif
#endif

let () = if !debug then
  !debug_print "Selected TLS library: %s\n"
    (Sexplib.Sexp.to_string (sexp_of_tls_lib !tls_library))

type +'a io = 'a Lwt.t
type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel

type client_tls_config =
  [ `Hostname of string ] *
  [ `IP of Ipaddr.t ] *
  [ `Port of int ]
[@@deriving sexp]

type client = [
  | `TLS of client_tls_config
  | `TLS_native of client_tls_config
  | `OpenSSL of client_tls_config
  | `TCP of [ `IP of Ipaddr.t ] * [`Port of int ]
  | `Unix_domain_socket of [ `File of string ]
  | `Vchan_direct of [ `Domid of int ] * [ `Port of string ]
  | `Vchan_domain_socket of [ `Domain_name of string ] * [ `Port of string ]
] [@@deriving sexp]

(** Configuration fragment for a listening TLS server *)
type server_tls_config =
  [ `Crt_file_path of string ] *
  [ `Key_file_path of string ] *
  [ `Password of bool -> string | `No_password ] *
  [ `Port of int ]
[@@deriving sexp]

(** Set of supported listening mechanisms that are supported by this module. *)
type server = [
  | `TLS of server_tls_config
  | `OpenSSL of server_tls_config
  | `TLS_native of server_tls_config
  | `TCP of [ `Port of int ]
  | `Unix_domain_socket of [ `File of string ]
  | `Vchan_direct of int * string
  | `Vchan_domain_socket of string  * string
  | `Launchd of string
] [@@deriving sexp]

type tls_server_key = [
  | `None
  | `TLS of
      [ `Crt_file_path of string ] *
      [ `Key_file_path of string ] *
      [ `Password of bool -> string | `No_password ]
] [@@deriving sexp]

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
  [%sexp_of: string option * tls_server_key ]
    ((match ctx.src with
      | None -> None
      | Some sa -> Some (string_of_unix_sockaddr sa)),
     ctx.tls_server_key)

type tcp_flow = {
  fd: Lwt_unix.file_descr sexp_opaque;
  ip: Ipaddr.t;
  port: int;
} [@@deriving sexp]

type domain_flow = {
  fd: Lwt_unix.file_descr sexp_opaque;
  path: string;
} [@@deriving sexp]

type vchan_flow = {
  domid: int;
  port: string;
} [@@deriving sexp]

type flow =
  | TCP of tcp_flow
  | Domain_socket of domain_flow
  | Vchan of vchan_flow
[@@deriving sexp]

let flow_of_fd fd sa =
  match sa with
  | Unix.ADDR_UNIX path -> Domain_socket { fd; path }
  | Unix.ADDR_INET (ip,port) -> TCP { fd; ip=Ipaddr_unix.of_inet_addr ip; port }

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

(* Vanilla sockaddr connection *)
module Sockaddr_client = struct
  let connect ?src sa =
    Conduit_lwt_server.with_socket sa (fun fd ->
        let () =
          match src with
          | None -> ()
          | Some src_sa -> Lwt_unix.bind fd src_sa
        in
        Lwt_unix.connect fd sa >>= fun () ->
        let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
        let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
        return (fd, ic, oc)
      )
end

module Sockaddr_server = struct
  open Conduit_lwt_server

  let init_socket sockaddr =
    Unix.handle_unix_error (fun () ->
      let sock = Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr)
                                 Unix.SOCK_STREAM 0 in
      Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
      Lwt_unix.bind sock sockaddr;
      Lwt_unix.listen sock 15;
      Lwt_unix.set_close_on_exec sock;
      sock) ()

  let process_accept ?timeout callback (client,peeraddr) =
    ( try
        Lwt_unix.setsockopt client Lwt_unix.TCP_NODELAY true
      with
        (* This is expected for Unix domain sockets *)
        | Unix.Unix_error(Unix.EOPNOTSUPP, _, _) -> ());
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input client in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.output client in
    let c = callback (flow_of_fd client peeraddr) ic oc in
    let events = match timeout with
      |None -> [c]
      |Some t -> [c; (Lwt_unix.sleep (float_of_int t)) ] in
    let _ = Lwt.pick events >>= fun () -> close (ic,oc) in
    return ()

  let init ~on ?(stop = fst (Lwt.wait ())) ?timeout callback =
    let cont = ref true in
    let s = match on with
    | `Socket s -> s
    | `Sockaddr sockaddr -> init_socket sockaddr in
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

(** TLS client connection functions *)

let connect_with_tls_native ~ctx (`Hostname hostname, `IP ip, `Port port) =
#if HAVE_LWT_TLS
  let sa = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip,port) in
  Conduit_lwt_tls.Client.connect ?src:ctx.src hostname sa
  >|= fun (fd, ic, oc) ->
  let flow = TCP { fd ; ip ; port } in
  (flow, ic, oc)
#else
   fail (Failure "No TLS support compiled into Conduit")
#endif

let connect_with_openssl ~ctx (`Hostname hostname, `IP ip, `Port port) =
#if HAVE_LWT_SSL
  let sa = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip,port) in
  Conduit_lwt_unix_ssl.Client.connect ?src:ctx.src sa
  >>= fun (fd, ic, oc) ->
  let flow = TCP {fd;ip;port} in
  return (flow, ic, oc)
#else
  fail (Failure "No SSL support compiled into Conduit")
#endif

let connect_with_default_tls ~ctx tls_client_config =
  match !tls_library with
  | OpenSSL -> connect_with_openssl ~ctx tls_client_config
  | Native -> connect_with_tls_native ~ctx tls_client_config
  | No_tls -> fail (Failure "No SSL or TLS support compiled into Conduit")

(** VChan connection functions *)
let connect_with_vchan_lwt ~ctx (`Domid domid, `Port sport) =
#if HAVE_VCHAN_LWT
  (match Vchan.Port.of_string sport with
   | `Error s -> fail (Failure ("Invalid vchan port: " ^ s))
   | `Ok p -> return p)
  >>= fun port ->
  let flow = Vchan { domid; port=sport } in
  Vchan_lwt_unix.open_client ~domid ~port () >>= fun (ic, oc) ->
  return (flow, ic, oc)
#else
  let _domid = domid in let _sport = sport in
  fail (Failure "No Vchan support compiled into Conduit")
#endif

(** Main connection function *)

let connect ~ctx (mode:client) =
  match mode with
  | `TCP (`IP ip, `Port port) ->
    let sa = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port) in
    Sockaddr_client.connect ?src:ctx.src sa
    >>= fun (fd, ic, oc) ->
    let flow = TCP {fd;ip;port} in
    return (flow, ic, oc)
  | `Unix_domain_socket (`File path) ->
    Sockaddr_client.connect (Unix.ADDR_UNIX path)
    >>= fun (fd, ic, oc) ->
    let flow = Domain_socket {fd; path} in
    return (flow, ic, oc)
  | `TLS c -> connect_with_default_tls ~ctx c
  | `OpenSSL c -> connect_with_openssl ~ctx c
  | `TLS_native c -> connect_with_tls_native ~ctx c
  | `Vchan_direct c -> connect_with_vchan_lwt ~ctx c
  | `Vchan_domain_socket _uuid ->
     fail (Failure "Vchan_domain_socket not implemented")

let sockaddr_on_tcp_port ctx port =
  let open Unix in
  match ctx.src with
  | Some (ADDR_UNIX _) -> raise (Failure "Cant listen to TCP on a domain socket")
  | Some (ADDR_INET (a,_)) -> ADDR_INET (a,port), Ipaddr_unix.of_inet_addr a
  | None -> ADDR_INET (inet_addr_any,port), Ipaddr.(V4 V4.any)

let serve_with_openssl ?timeout ?stop ~ctx ~certfile ~keyfile
                       ~pass ~port callback t =
#if HAVE_LWT_SSL
  let sockaddr, ip = sockaddr_on_tcp_port ctx port in
  let password =
    match pass with
    | `No_password -> None
    | `Password fn -> Some fn
  in
  Conduit_lwt_unix_ssl.Server.init
    ?password ~certfile ~keyfile ?timeout ?stop sockaddr
    (fun fd ic oc -> callback (TCP {fd;ip;port}) ic oc) >>= fun () ->
  t
#else
  fail (Failure "No SSL support compiled into Conduit")
#endif

let serve_with_tls_native ?timeout ?stop ~ctx ~certfile ~keyfile
                          ~pass ~port callback t =
#if HAVE_LWT_TLS
  let sockaddr, ip = sockaddr_on_tcp_port ctx port in
  (match pass with
    | `No_password -> return ()
    | `Password _ -> fail (Failure "OCaml-TLS cannot handle encrypted pem files")
  ) >>= fun () ->
  Conduit_lwt_tls.Server.init
    ~certfile ~keyfile ?timeout ?stop sockaddr
    (fun fd ic oc -> callback (TCP {fd;ip;port}) ic oc)
  >>= fun () -> t
#else
  fail (Failure "No TLS support compiled into Conduit")
#endif

let serve_with_default_tls ?timeout ?stop ~ctx ~certfile ~keyfile
                           ~pass ~port callback t =
  match !tls_library with
  | OpenSSL -> serve_with_openssl ?timeout ?stop ~ctx ~certfile ~keyfile
                                 ~pass ~port callback t
  | Native -> serve_with_tls_native ?timeout ?stop ~ctx ~certfile ~keyfile
                                   ~pass ~port callback t
  | No_tls -> fail (Failure "No SSL or TLS support compiled into Conduit")

let serve ?timeout ?stop ~(ctx:ctx) ~(mode:server) callback =
  let t, _u = Lwt.task () in (* End this via Lwt.cancel *)
  let callback flow ic oc = Lwt.catch
    (fun () -> callback flow ic oc)
    (fun exn -> !Lwt.async_exception_hook exn; Lwt.return_unit)
  in
  match mode with
  | `TCP (`Port port) ->
       let sockaddr, ip = sockaddr_on_tcp_port ctx port in
       Sockaddr_server.init ~on:(`Sockaddr sockaddr) ?timeout ?stop callback
       >>= fun () -> t
  | `Unix_domain_socket (`File path) ->
       let sockaddr = Unix.ADDR_UNIX path in
       Sockaddr_server.init ~on:(`Sockaddr sockaddr) ?timeout ?stop callback
       >>= fun () -> t
  | `TLS (`Crt_file_path certfile, `Key_file_path keyfile, pass, `Port port) ->
     serve_with_default_tls ?timeout ?stop ~ctx ~certfile ~keyfile
                            ~pass ~port callback t
  | `OpenSSL (`Crt_file_path certfile, `Key_file_path keyfile,
              pass, `Port port) ->
     serve_with_openssl ?timeout ?stop ~ctx ~certfile ~keyfile
                        ~pass ~port callback t
  | `TLS_native (`Crt_file_path certfile, `Key_file_path keyfile,
                 pass, `Port port) ->
     serve_with_tls_native ?timeout ?stop ~ctx ~certfile ~keyfile
                           ~pass ~port callback t
  |`Vchan_direct (domid, sport) ->
#if HAVE_VCHAN_LWT
    begin match Vchan.Port.of_string sport with
      | `Error s -> fail (Failure ("Invalid vchan port: " ^ s))
      | `Ok p -> return p
    end >>= fun port ->
    Vchan_lwt_unix.open_server ~domid ~port () >>= fun (ic, oc) ->
    callback (Vchan {domid; port=sport}) ic oc
#else
    fail (Failure "No Vchan support compiled into Conduit")
#endif
  | `Vchan_domain_socket _uuid ->
    fail (Failure "Vchan_domain_socket not implemented")
  | `Launchd name ->
#if HAVE_LAUNCHD_LWT
    Lwt_launchd.activate_socket name
    >>= fun sockets ->
    begin match (Launchd.error_to_msg sockets) with
    | Result.Ok sockets ->
      Lwt_list.iter_p
        (fun s ->
          Sockaddr_server.init ~on:(`Socket s) ?timeout ?stop callback
        ) sockets
    | Result.Error (`Msg m) ->
      fail (Failure m)
    end >>= fun () -> t
#else
    fail (Failure "No Launchd support compiled into Conduit")
#endif

let endp_of_flow = function
  | TCP { ip; port; _ } -> `TCP (ip, port)
  | Domain_socket { path; _ } -> `Unix_domain_socket path
  | Vchan { domid; port } -> `Vchan_direct (domid, port)

(** Use the configuration of the server to interpret how to
    handle a particular endpoint from the resolver into a
    concrete implementation of type [client] *)
let endp_to_client ~ctx (endp:Conduit.endp) : client Lwt.t =
  match endp with
  | `TCP (ip, port) -> return (`TCP (`IP ip, `Port port))
  | `Unix_domain_socket file -> return (`Unix_domain_socket (`File file))
  | `Vchan_direct (domid, port) ->
     return (`Vchan_direct (`Domid domid, `Port port))
  | `Vchan_domain_socket (name, port) ->
     return (`Vchan_domain_socket (`Domain_name name, `Port port))
  | `TLS (host, (`TCP (ip, port))) ->
     return (`TLS (`Hostname host, `IP ip, `Port port))
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
