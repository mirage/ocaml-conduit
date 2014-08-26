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

type client = [
  | `TCP of Ipaddr.t * int
  | `Vchan of string list
] with sexp

type server = [
  | `TCP of [ `Port of int ]
  | `Vchan of string list
] with sexp

(** All the possible connection types supported *)
module Make_flow(S:V1_LWT.STACKV4) = struct

  type 'a io = 'a Lwt.t
  type error = string (* XXX *)
  type buffer = Cstruct.t

  type flow =
    | TCPv4 of S.TCPV4.flow
    | Vchan of string list (* TODO *)

  let of_tcpv4 f = TCPv4 f
  let of_vchan v = Vchan v

  let read flow =
    match flow with
    | Vchan _ -> fail (Failure "TODO")
    | TCPv4 t ->
        S.TCPV4.read t >>= function
        | `Ok _ | `Eof as r -> return r
        | `Error _ -> return (`Error "read") (* XXX *)

  let write flow buf =
    match flow with
    | Vchan _ -> fail (Failure "TODO")
    | TCPv4 t ->
        S.TCPV4.write t buf >>= fun () ->
        return (`Ok ())

  let writev flow bufv =
    match flow with
    | Vchan _ -> fail (Failure "TODO")
    | TCPv4 t ->
        S.TCPV4.writev t bufv >>= fun () ->
        return (`Ok ())
end

module Make(S:V1_LWT.STACKV4) = struct

  module Flow = Make_flow(S)
  type +'a io = 'a Lwt.t
  type ic = Flow.flow
  type oc = Flow.flow
  type conn = Flow.flow

  type ctx = {
    stack: S.t;
  }

  let init stack =
    return { stack }

  let connect ~ctx ~mode =
    match mode with
    | `Vchan _path ->
      fail (Failure "No Vchan support compiled into Conduit")
    | `TCP (Ipaddr.V6 _ip, _port) ->
      fail (Failure "No IPv6 support compiled into Conduit")
    | `TCP (Ipaddr.V4 ip, port) ->
      let tcp = S.tcpv4 ctx.stack in
      S.TCPV4.create_connection tcp (ip,port)
      >>= function
      | `Error _err -> fail (Failure "connection failed")
      | `Ok flow ->
           let flow = Flow.of_tcpv4 flow in
           return (flow, flow, flow)

  let serve ?(timeout=60) ?stop ~ctx ~mode fn =
    match mode with
    |`TCP (`Port port) ->
      S.listen_tcpv4 ctx.stack ~port
        (fun flow ->
           let f = Flow.of_tcpv4 flow in fn f f f);
      (* TODO: use stop function *)
      return ()
    |`Vchan path ->
      let _f = Flow.of_vchan path in
      fail (Failure "vchan not implemented")
end

(*
  let connect_to_uri ~ctx uri =
    >>= function
    | `TCP (_ip,_port) as mode -> connect ~ctx mode
    | `Unix_domain_socket _path as mode -> connect ~ctx mode
    | `TLS (host, `TCP (ip, port)) -> connect ~ctx (`OpenSSL (host, ip, port))
    | `TLS (_host, _) -> fail (Failure "TLS to non-TCP unsupported")
    | `Vchan _path -> fail (Failure "VChan not supported")
    | `Unknown err -> fail (Failure ("resolution failed: " ^ err))
*)
