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

type vchan_port = Vchan.Port.t with sexp

type client = [
  | `TCP of Ipaddr.t * int
  | `Vchan_direct of int * vchan_port
  | `Vchan_domain_socket of [ `Uuid of string ] * [ `Port of vchan_port ]
] with sexp

type server = [
  | `TLS of Tls.Config.server * server
  | `TCP of [ `Port of int ]
  | `Vchan_direct of [`Remote_domid of int] * vchan_port
  | `Vchan_domain_socket of [ `Uuid of string ] * [ `Port of vchan_port ]
] with sexp

type unknown = [ `Unknown of string ]

module type VCHAN_FLOW = V1_LWT.FLOW
  with type error := unknown

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

module type ENDPOINT = sig
  type t with sexp_of
  type port = vchan_port

  type error = [
    `Unknown of string
  ]

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

module type PEER = sig
  type t with sexp_of
  type flow with sexp_of
  type uuid with sexp_of
  type port with sexp_of

  module Endpoint : ENDPOINT

  val register : uuid -> t Lwt.t

  val listen : t -> Conduit.endp Lwt_stream.t Lwt.t

  val connect : t -> remote_name:uuid -> port:port -> Conduit.endp Lwt.t

end

module type VCHAN_PEER = PEER
  with type uuid = string
   and type port = vchan_port

module type TLS = sig
  module FLOW : V1_LWT.FLOW   (* Underlying (encrypted) flow *)
    with type flow = Dynamic_flow.flow
  include V1_LWT.FLOW
  type tracer
  val server_of_flow :
    ?trace:tracer ->
    Tls.Config.server -> FLOW.flow ->
    [> `Ok of flow | `Error of error | `Eof  ] Lwt.t
end

module No_TLS : TLS = struct
  module FLOW = Dynamic_flow
  include FLOW
  type tracer = unit
  let server_of_flow ?trace:_ _config _underlying =
    return (`Error (fun () -> "No_TLS: TLS support for Conduit is disabled"))
end

module Make(S:V1_LWT.STACKV4)(V:VCHAN_PEER)(TLS:TLS) = struct

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

  let endp_to_client ~ctx:_ (endp:Conduit.endp) : client Lwt.t =
    match endp with
    | `TCP (_ip, _port) as mode -> return mode
    | `Vchan_direct (domid, port) ->
       begin
         match Vchan.Port.of_string port with
         | `Error s -> fail (Failure ("Invalid vchan port: " ^ s))
         | `Ok p -> return p
       end >>= fun port ->
       return (`Vchan_direct (domid, port))
    | `Vchan_domain_socket (uuid,  port) ->
       begin
         match Vchan.Port.of_string port with
         | `Error s -> fail (Failure ("Invalid vchan port: " ^ s))
         | `Ok p -> return p
       end >>= fun port ->
       return (`Vchan_domain_socket (`Uuid uuid, `Port port))
    | `Unix_domain_socket _path ->
       fail (Failure "Domain sockets not valid on Mirage")
    | `TLS (_host, _) -> fail (Failure "TLS currently unsupported")
    | `Unknown err -> fail (Failure ("resolution failed: " ^ err))

  let endp_to_server ~ctx:_ (endp:Conduit.endp) : server Lwt.t =
    match endp with
    | `TCP (_ip, port) -> return (`TCP (`Port port))
    | `Vchan_direct (domid, port) ->
       begin
         match Vchan.Port.of_string port with
         | `Error s -> fail (Failure ("Invalid vchan port: " ^ s))
         | `Ok p -> return p
       end >>= fun port ->
       return (`Vchan_direct ((`Remote_domid domid), port))
    | `Vchan_domain_socket (uuid,  port) ->
       begin
         match Vchan.Port.of_string port with
         | `Error s -> fail (Failure ("Invalid vchan port: " ^ s))
         | `Ok p -> return p
       end >>= fun port ->
       return (`Vchan_domain_socket (`Uuid uuid, `Port port))
    | `Unix_domain_socket _path ->
       fail (Failure "Domain sockets not valid on Mirage")
    | `TLS (_host, _) -> fail (Failure "TLS currently unsupported")
    | `Unknown err -> fail (Failure ("resolution failed: " ^ err))

  let init ?peer ?stack () =
    return { peer; stack }

  let default_ctx =
    { peer = None; stack = None }

  let rec connect ~ctx (mode:client) =
    match mode, ctx.stack with
    | `Vchan_domain_socket (`Uuid uuid, `Port port), _ -> begin
       match ctx.peer with
       | None -> fail (Failure "TODO")
       | Some peer ->
           V.connect peer ~remote_name:uuid ~port
           >>= fun endp ->
           endp_to_client ~ctx endp
           >>= fun client ->
           connect ~ctx client
    end
    | `Vchan_direct (domid, port), _ ->
      Printf.printf "Conduit.connect: Vchan %d %s\n%!"
                    domid (Vchan.Port.to_string port);
      V.Endpoint.client ~domid ~port ()
      >>= fun flow ->
      Printf.printf "Conduit.connect: connected!\n%!";
      let flow = Dynamic_flow.Flow ((module V.Endpoint), flow) in
      return (flow, flow, flow)
    | `TCP (Ipaddr.V6 _ip, _port), _ ->
      fail (Failure "No IPv6 support compiled into Conduit")
    | `TCP (Ipaddr.V4 _ip, _port), None ->
      fail (Failure "No stack bound to Conduit")
    | `TCP (Ipaddr.V4 ip, port), Some tcp  ->
      S.TCPV4.create_connection (S.tcpv4 tcp) (ip,port) >>= function
      | `Error _err -> fail (Failure "connection failed")
      | `Ok flow ->
        let flow = Dynamic_flow.Flow ((module S.TCPV4), flow) in
        return (flow, flow, flow)

  let rec serve ?(timeout=60) ?stop ~ctx ~(mode:server) fn =
    let _ = timeout in
    let t, _u = Lwt.task () in
    Lwt.on_cancel t (fun () -> print_endline "Stopping server thread");
    match mode, ctx.stack with
    | `Vchan_domain_socket (`Uuid uuid, `Port port), _ -> begin
      match ctx.peer with
      | None -> fail (Failure "TODO")
      | Some peer ->
         V.listen peer
         >>= fun conns ->
         Lwt_stream.iter_p (fun endp ->
           endp_to_server ~ctx endp
           >>= fun server ->
           match server with
           | `Vchan_direct (`Remote_domid domid, port) ->
              V.Endpoint.server ~domid ~port ()
              >>= fun t ->
              let f = Dynamic_flow.Flow ((module V.Endpoint), t) in
              fn f f f
           | _ -> fail (Failure "TODO")
         ) conns
    end
    |`TCP (`Port _port), None ->
      fail (Failure "No stack bound to Conduit")
    |`TCP (`Port port), Some stack ->
      S.listen_tcpv4 stack ~port
        (fun flow ->
           let f = Dynamic_flow.Flow ((module S.TCPV4), flow) in
           fn f f f
        );
      t
    |`Vchan_direct (`Remote_domid domid, port), _ ->
       V.Endpoint.server ~domid ~port ()
       >>= fun t ->
       let f = Dynamic_flow.Flow ((module V.Endpoint), t) in
       fn f f f
    |`TLS (config, underlying), _ ->
        serve ~timeout ?stop ~ctx ~mode:underlying (fun f _ _ ->
          TLS.server_of_flow config f >>= function
          | `Error err -> fail (Failure (TLS.error_message err))
          | `Eof -> fail (Failure "End-of-file from TLS.server_of_flow")
          | `Ok underlying ->
              let flow = Dynamic_flow.Flow ((module TLS), underlying) in
              fn flow flow flow (* XXX: why in triplicate? *)
        )
end

module type S = sig

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
