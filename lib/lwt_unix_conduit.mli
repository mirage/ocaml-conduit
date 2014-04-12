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

type 'a io = 'a Lwt.t
type ic = Lwt_io.input_channel
type oc = Lwt_io.output_channel

type server_mode = [
  | `SSL of [ `Crt_file_path of string ] * [ `Key_file_path of string ]
  | `TCP 
] with sexp

val build_sockaddr : string -> int -> Lwt_unix.sockaddr io

val connect :
  mode:[< `SSL | `TCP ] ->
  host:string -> service:string -> unit -> (ic * oc) io

val serve :
  mode:server_mode ->
  sockaddr:Lwt_unix.sockaddr ->
  ?timeout:int -> (ic -> oc -> unit io) -> unit io

val close_in : 'a Lwt_io.channel -> unit
val close_out : 'a Lwt_io.channel -> unit
val close : 'a Lwt_io.channel -> 'b Lwt_io.channel -> unit
