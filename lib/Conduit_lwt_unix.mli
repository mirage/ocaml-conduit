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

type client = [
  | `OpenSSL of string * Ipaddr.t * int
  | `TCP of Ipaddr.t * int
  | `Unix_domain_socket of string
] with sexp

type server = [
  | `OpenSSL of
      [ `Crt_file_path of string ] *
      [ `Key_file_path of string ] *
      [ `Password of bool -> string | `No_password ] *
      [ `Port of int ]
  | `TCP of [ `Port of int ]
  | `Unix_domain_socket of [ `File of string ]
] with sexp

type 'a io = 'a Lwt.t
type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel
type flow with sexp

type tls_server_key = [
 | `None
 | `OpenSSL of
    [ `Crt_file_path of string ] *
    [ `Key_file_path of string ] *
    [ `Password of bool -> string | `No_password ]
]

type ctx
val init : ?src:string -> ?tls_server_key:tls_server_key -> unit -> ctx io
val default_ctx : ctx

val connect : ctx:ctx -> client -> (flow * ic * oc) io

val serve :
  ?timeout:int -> ?stop:(unit io) -> ctx:ctx ->
   mode:server -> (flow -> ic -> oc -> unit io) -> unit io

val endp_of_flow : flow -> Conduit.endp
val endp_to_client : ctx:ctx -> Conduit.endp -> client io
val endp_to_server : ctx:ctx -> Conduit.endp -> server io

