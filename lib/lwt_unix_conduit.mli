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

type ctx
val init : ?src:string -> unit -> ctx io

module Client : sig

  type t = [
    | `SSL of string * int
    | `TCP of string * int
    | `Domain_socket of string
  ]

  val connect : ctx -> t -> (ic * oc) io
end

module Server : sig

  type t = [
    | `SSL of 
       [ `Crt_file_path of string ] * 
       [ `Key_file_path of string ] *
       [ `Password of bool -> string | `No_password ] *
       [ `Port of int]
    | `TCP of [ `Port of int ]
  ] with sexp

  val serve : ?timeout:int -> ctx -> t -> (ic -> oc -> unit io) -> unit io
end

val close_in : 'a Lwt_io.channel -> unit
val close_out : 'a Lwt_io.channel -> unit
val close : 'a Lwt_io.channel -> 'b Lwt_io.channel -> unit
