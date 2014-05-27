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

module Client : sig

  type t = [
    | `SSL of string * int
    | `TCP of string * int
    | `Domain_socket of string
  ] with sexp

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

end

val has_async_ssl : bool
(** [has_async_ssl] is [true] if Async SSL support has been compiled into
    this library. *)

val has_lwt_unix_ssl : bool
(** [has_lwt_ssl] is [true] if Lwt SSL support has been compiled into
    this library. *)
