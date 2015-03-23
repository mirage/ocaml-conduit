(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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
open Sexplib.Conv

(** All the possible connection types supported *)
module Dynamic_flow = struct

  type 'a io = 'a Lwt.t
  type error = unit -> string
  type buffer = Cstruct.t

  type flow =
    | Flow : (module V1_LWT.FLOW with
                type flow = 'a) * 'a -> flow

  let error_message fn = fn ()
  let wrap_errors (type e) (module F : V1_LWT.FLOW with type error = e) v =
    v >>= function
    | `Error (err : e) -> return (`Error (fun () -> F.error_message err))
    | `Ok _ | `Eof as other -> return other

  let read (Flow ((module F), flow)) = wrap_errors (module F) (F.read flow)
  let write (Flow ((module F), flow)) b = wrap_errors (module F) (F.write flow b)
  let writev (Flow ((module F), flow)) b = wrap_errors (module F) (F.writev flow b)
  let close (Flow ((module F), flow)) = F.close flow
end

module type VCHAN = sig
  type t with sexp_of
  type flow with sexp_of
  type uuid = string
  type port with sexp
  val port_of_string: string -> [`Ok of port | `Error of string]
  module Endpoint : sig
    type t
    type error = [`Unknown of string]

    val server :
      domid:int ->
      port:port ->
      ?read_size:int ->
      ?write_size:int ->
      unit -> t Lwt.t

    val client :
      domid:int ->
      port:port ->
      unit -> t Lwt.t

    include V1_LWT.FLOW
      with type flow = t
       and  type error := error
       and  type 'a io = 'a Lwt.t
       and  type buffer = Cstruct.t
  end

  val register : uuid -> t Lwt.t

  val listen : t -> Conduit.endp Lwt_stream.t Lwt.t

  val connect : t -> remote_name:uuid -> port:port -> Conduit.endp Lwt.t

end

module type TLS = sig
  module FLOW : V1_LWT.FLOW   (* Underlying (encrypted) flow *)
    with type flow = Dynamic_flow.flow
  include V1_LWT.FLOW
  type client with sexp
  type server with sexp
  type tracer
  val server_of_flow : ?trace:tracer -> server -> FLOW.flow ->
    [> `Ok of flow | `Error of error | `Eof  ] Lwt.t
  val client_of_flow: client -> FLOW.flow ->
    [> `Ok of flow | `Error of error | `Eof] Lwt.t
end

module No_TLS: TLS = struct
  module FLOW = Dynamic_flow
  include FLOW
  type client = unit with sexp
  type server = unit with sexp
  type tracer = unit
  let error () =
    return (`Error (fun () -> "No_TLS: TLS support for Conduit is disabled"))
  let server_of_flow ?trace:_ _config _underlying = error ()
  let client_of_flow _config _underlying = error ()
end

module No_Vchan: VCHAN = struct
  type t = unit with sexp_of
  type flow = unit with sexp_of
  type uuid = string with sexp_of
  type port = unit with sexp
  let err_not_supported () = Lwt.fail (Failure "not supported")
  let port_of_string _ = `Error "not supported"
  let register _ = Lwt.return ()
  let listen _ = Lwt.fail (Failure "not supported")
  let connect _ ~remote_name:_ ~port:_ = Lwt.return (`Unknown "not supported")
  module Endpoint = struct
    type t = unit
    type error = [`Unknown of string]
    let server ~domid:_ ~port:_ ?read_size:_ ?write_size:_ () =
      err_not_supported ()
    let client ~domid:_ ~port:_ () = err_not_supported ()
    type 'a io = 'a Lwt.t
    type buffer = Cstruct.t
    type flow = unit
    let error_message = function `Unknown x -> x
    let read _ = err_not_supported ()
    let write _ _ = err_not_supported ()
    let writev _ _ = err_not_supported ()
    let close _ = err_not_supported ()
  end
end

module Make(S:V1_LWT.STACKV4)(V:VCHAN)(TLS:TLS) = struct

  type vchan_port = V.port with sexp
  type tls_client = TLS.client with sexp
  type tls_server = TLS.server with sexp

  type client = [
    | `TLS of tls_client * client
    | `TCP of Ipaddr.t * int
    | `Vchan_direct of int * vchan_port
    | `Vchan_domain_socket of [ `Uuid of string ] * [ `Port of vchan_port ]
  ] with sexp

  type server = [
    | `TLS of tls_server * server
    | `TCP of [ `Port of int ]
    | `Vchan_direct of [`Remote_domid of int] * vchan_port
    | `Vchan_domain_socket of [ `Uuid of string ] * [ `Port of vchan_port ]
  ] with sexp

  module Flow = Dynamic_flow

  type +'a io = 'a Lwt.t
  type ic = Flow.flow
  type oc = Flow.flow
  type flow = Flow.flow
  type stack = S.t
  type peer = V.t

  type ctx = {
    peer: V.t option;
    stack: S.t option sexp_opaque;
  } with sexp_of

  let fail fmt = Printf.ksprintf (fun s -> fail (Failure s)) fmt
  let err_ipv6 = fail "%s: No IPv6 support compiled into Conduit"
  let err_no_stack () =  fail "No TCP stack bound to Conduit"
  let err_tcp e = fail "TCP connection failed: %s" (S.TCPV4.error_message e)
  let err_tls e = fail "TLS connection failed: %s" (TLS.error_message e)
  let err_eof = fail "%s: End-of-file!"
  let err_tls_not_supported = fail "%s: TLS is not supported"
  let err_vchan_port = fail "%s: invalide Vchan port"
  let err_domain_socks () =
    fail "Unix domain sockets are not supported inside Unikernels"
  let err_resolution_failed = fail "%s: resolution failed"
  let err_todo = fail "%s: TODO"

  let vchan_port_of_string port =
    match V.port_of_string port with
    | `Error s -> err_vchan_port s
    | `Ok p    -> return p

  let endp_to_client ~ctx:_ (endp:Conduit.endp) : client Lwt.t =
    match endp with
    | `TLS _ -> err_tls_not_supported "endp_to_client"
    | `TCP _ as mode -> return mode
    | `Vchan_direct (domid, port) ->
      vchan_port_of_string port >>= fun port ->
      return (`Vchan_direct (domid, port))
    | `Vchan_domain_socket (uuid,  port) ->
      vchan_port_of_string port >>= fun port ->
      return (`Vchan_domain_socket (`Uuid uuid, `Port port))
    | `Unix_domain_socket _ -> err_domain_socks ()
    | `Unknown err -> err_resolution_failed err

  let endp_to_server ~ctx:_ (endp:Conduit.endp) : server Lwt.t =
    match endp with
    | `TLS _ -> err_tls_not_supported "endp_to_server"
    | `TCP (_, port) -> return (`TCP (`Port port))
    | `Vchan_direct (domid, port) ->
      vchan_port_of_string port >>= fun port ->
      return (`Vchan_direct ((`Remote_domid domid), port))
    | `Vchan_domain_socket (uuid,  port) ->
      vchan_port_of_string port >>= fun port ->
      return (`Vchan_domain_socket (`Uuid uuid, `Port port))
    | `Unix_domain_socket _ -> err_domain_socks ()
    | `Unknown err -> err_resolution_failed err

  let init ?peer ?stack () =
    return { peer; stack }

  let default_ctx =
    { peer = None; stack = None }

  let connect_vchan_domain_socket ~ctx connect uuid port = match ctx.peer with
    | None      -> err_todo "connect_vchan"
    | Some peer ->
      V.connect peer ~remote_name:uuid ~port  >>= fun endp ->
      endp_to_client ~ctx endp >>= fun client ->
      connect ~ctx client

  let connect_vchan_direct domid port =
    V.Endpoint.client ~domid ~port () >>= fun flow ->
    let flow = Dynamic_flow.Flow ((module V.Endpoint), flow) in
    return (flow, flow, flow)

  let connect_tcpv4 tcp ip port =
    S.TCPV4.create_connection (S.tcpv4 tcp) (ip, port) >>= function
    | `Error e -> err_tcp e
    | `Ok flow ->
      let flow = Dynamic_flow.Flow ((module S.TCPV4), flow) in
      return (flow, flow, flow)

  let connect_tls ~ctx connect config underlying =
    connect ~ctx underlying >>= fun (flow, _, _) ->
    TLS.client_of_flow config flow >>= function
    | `Error e -> err_tls e
    | `Eof     -> err_eof "connect_tls"
    | `Ok flow ->
      let flow = Dynamic_flow.Flow ((module TLS), flow) in
      return (flow, flow, flow)

  let rec connect ~ctx (mode:client) =
    match mode, ctx.stack with
    | `Vchan_domain_socket (`Uuid id, `Port p), _ ->
      connect_vchan_domain_socket ~ctx connect id p
    | `Vchan_direct (domid, port), _ -> connect_vchan_direct domid port
    | `TCP (Ipaddr.V6 _, _), _ -> err_ipv6 "TCP"
    | `TCP (Ipaddr.V4 _, _), None -> err_no_stack ()
    | `TCP (Ipaddr.V4 ip, port), Some tcp  -> connect_tcpv4 tcp ip port
    | `TLS (config, mode), _ -> connect_tls ~ctx connect config mode

  let serve_vchan_domain_sockets ~ctx _connect fn =
    match ctx.peer with
    | None      -> err_todo "serve_vchan_domain_sockets"
    | Some peer ->
      V.listen peer >>= fun conns ->
      Lwt_stream.iter_p (fun endp ->
          endp_to_server ~ctx endp >>= function
          | `Vchan_direct (`Remote_domid domid, port) ->
            V.Endpoint.server ~domid ~port () >>= fun t ->
            let f = Dynamic_flow.Flow ((module V.Endpoint), t) in
            fn f f f
          | _ -> err_todo "serve_vchan_domain_sockets"
        ) conns

  let serve_vchan_direct domid port fn =
    V.Endpoint.server ~domid ~port () >>= fun t ->
    let f = Dynamic_flow.Flow ((module V.Endpoint), t) in
    fn f f f

  let serve_tcpv4 stack port fn =
    let t, _u = Lwt.task () in
    Lwt.on_cancel t (fun () -> print_endline "Stopping server thread");
    S.listen_tcpv4 stack ~port (fun flow ->
        let f = Dynamic_flow.Flow ((module S.TCPV4), flow) in
        fn f f f
      );
    t

  let serve_tls config fn flow _ _ =
    TLS.server_of_flow config flow >>= function
    | `Error err -> err_tls err
    | `Eof -> err_eof "TLS.server_of_flow"
    | `Ok underlying ->
      let flow = Dynamic_flow.Flow ((module TLS), underlying) in
      fn flow flow flow (* XXX: why in triplicate? *)

  let rec serve ?(timeout=60) ?stop ~ctx ~(mode:server) fn =
    let _ = timeout in
    match mode, ctx.stack with
    | `Vchan_domain_socket _, _ -> serve_vchan_domain_sockets ~ctx connect fn
    | `TCP (`Port _port), None -> err_no_stack ()
    | `TCP (`Port port), Some stack -> serve_tcpv4 stack port fn
    | `Vchan_direct (`Remote_domid id, p), _ -> serve_vchan_direct id p fn
    | `TLS (config, underlying), _ ->
      serve ~timeout ?stop ~ctx ~mode:underlying (serve_tls config fn)

end

module type S = sig
  type vchan_port
  type tls_client
  type tls_server
  type client = [
    | `TLS of tls_client * client
    | `TCP of Ipaddr.t * int     (** IP address and TCP port number *)
    | `Vchan_direct of int * vchan_port (** Remote Xen domain id and port name *)
    | `Vchan_domain_socket of [ `Uuid of string ] * [ `Port of vchan_port ]
  ] with sexp
  type server = [
    | `TLS of tls_server * server
    | `TCP of [ `Port of int ]
    | `Vchan_direct of [ `Remote_domid of int ] * vchan_port
    | `Vchan_domain_socket of [ `Uuid of string ] * [ `Port of vchan_port ]
  ] with sexp
  module Flow : V1_LWT.FLOW
  type +'a io = 'a Lwt.t
  type ic = Flow.flow
  type oc = Flow.flow
  type flow = Flow.flow
  type stack
  type peer

  type ctx with sexp_of
  val default_ctx : ctx

  val init : ?peer:peer -> ?stack:stack -> unit -> ctx io

  val connect : ctx:ctx -> client -> (flow * ic * oc) io

  val serve :
    ?timeout:int -> ?stop:(unit io) -> ctx:ctx ->
     mode:server -> (flow -> ic -> oc -> unit io) -> unit io

  val endp_to_client: ctx:ctx -> Conduit.endp -> client io
  val endp_to_server: ctx:ctx -> Conduit.endp -> server io
end
