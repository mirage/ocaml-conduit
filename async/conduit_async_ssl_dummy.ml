(*
 * Copyright (c) 2012-2017 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 Clark Gaebel
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

module Ssl_config = struct
  type config = [`Ssl_not_compiled_in]

  let verify_certificate _ =
    failwith "Ssl not available, recompile with Async_ssl"

  let configure ?version:_ ?name:_ ?ca_file:_ ?ca_path:_ ?session:_ ?verify:_ () =
    failwith "Ssl not available, recompile with Async_ssl"
end

let ssl_connect _cfg _r _w =
  failwith "Ssl not available, recompile with Async_ssl"

let ssl_listen ?version:_ ?ca_file:_ ?ca_path:_ ~crt_file:_ ~key_file:_ _ _ =
  failwith "Ssl not available, recompile with Async_ssl"
