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

module Make(S:V1_LWT.STACKV4)(DNS:Dns_resolver_mirage.S with type stack=S.t) = struct
  type +'a io = 'a Lwt.t
  type ic = S.TCPV4.flow
  type oc = S.TCPV4.flow

  type ctx = {
    resolver: Lwt_conduit_resolver.t;
    dns: DNS.t;
    stack: S.t;
  }

  let init resolver stack =
    let dns = DNS.create stack in
    return { dns; resolver; stack }

  type conn = [
    | `TCPv4 of S.TCPV4.flow
  ]

  type endp = Ipaddr.t

  let peername conn =
    match conn with
    | `TCPv4 _fd -> raise (Failure "TODO")

  let sockname conn =
    match conn with
    | `TCPv4 _fd -> raise (Failure "TODO")

  let connect ~ctx (mode:Conduit.Client.t) =
    match mode with
    | `OpenSSL (_host, _ip, _port) -> 
      fail (Failure "No SSL support compiled into Conduit")
    | `Unix_domain_socket _file ->
      fail (Failure "No Unix support compiled into Conduit")
    | `TCP (Ipaddr.V6 _ip, _port) ->
      fail (Failure "No IPv6 support compiled into Conduit")
    | `TCP (Ipaddr.V4 ip, port) ->
      let tcp = S.tcpv4 ctx.stack in
      S.TCPV4.create_connection tcp (ip,port)
      >>= function
      | `Error _err -> fail (Failure "connection failed")
      | `Ok oc -> return (`TCPv4 oc, oc, oc)

  let connect_to_uri ~ctx uri =
    Lwt_conduit_resolver.resolve_uri ~uri ctx.resolver
    >>= function
    | `TCP (_ip,_port) as mode -> connect ~ctx mode
    | `Unix_domain_socket _path as mode -> connect ~ctx mode
    | `TLS (host, `TCP (ip, port)) -> connect ~ctx (`OpenSSL (host, ip, port))
    | `TLS (_host, _) -> fail (Failure "TLS to non-TCP unsupported")
    | `Vchan _path -> fail (Failure "VChan not supported")
    | `Unknown err -> fail (Failure ("resolution failed: " ^ err))

  let serve ?timeout ?ctx ?stop _mode _fn = fail (Failure "TODO")
end
