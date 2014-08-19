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

module Make(S:V1_LWT.STACKV4)(DNS:Dns_resolver_mirage.S with type stack=S.t) : sig
  type 'a io = 'a Lwt.t
  type ic = S.TCPV4.flow
  type oc = S.TCPV4.flow
  type endp = Ipaddr.t

  type ctx
  val init : Lwt_conduit_resolver.t -> S.t -> ctx io

  (** An individual connection *)

  type conn
  val peername : conn -> endp
  val sockname : conn -> endp

  val connect : ctx:ctx -> Conduit.Client.t -> (conn * ic * oc) io
  val connect_to_uri : ctx:ctx -> Uri.t -> (conn * ic * oc) io

  val serve : ?timeout:int -> ?ctx:ctx -> ?stop:(unit Lwt.t) ->
      Conduit.Server.t -> (conn -> ic -> oc -> unit io) -> unit io
end
