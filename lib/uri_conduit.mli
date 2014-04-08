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

module type T = sig

  type +'a io

  type ic

  type oc

  val connect_uri : 
    ?interrupt:unit io -> 
    Uri.t -> (ic * oc) io

  val connect : 
    ?interrupt:unit io -> 
    ?ssl:bool -> 
    host:string -> 
    service:string -> 
    unit -> (ic * oc) io

  val close_in : ic -> unit

  val close_out : oc -> unit

  val close : ic -> oc -> unit
end

val has_async_ssl : bool
