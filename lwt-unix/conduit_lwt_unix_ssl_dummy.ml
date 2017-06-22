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

open Lwt.Infix

module Client = struct
  let default_ctx = `Ssl_not_available

  let create_ctx ?certfile ?keyfile ?password () = default_ctx

  let connect ?(ctx=default_ctx) ?src sa = Lwt.fail_with "Ssl not available"
end

module Server = struct

  let default_ctx = `Ssl_not_available

  let listen ?(ctx=default_ctx) ?backlog ?password ~certfile ~keyfile sa =
    Lwt.fail_with "Ssl not available"

  let init ?(ctx=default_ctx) ?backlog ?password ~certfile ~keyfile ?stop
      ?timeout sa cb =
    Lwt.fail_with "Ssl not available"
end
