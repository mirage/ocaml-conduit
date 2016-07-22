(*
 * Copyright (c) 2016 Anil Madhavapeddy <anil@recoil.org>
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

open Cmdliner

let mode =
  let host =
    let doc = "IP address to listen on." in
    Arg.(value & opt string "0.0.0.0" & info ["l";"listen-addr"] ~docv:"HOST" ~doc)
  in
  let port =
    let doc = "TCP port to listen on." in
    Arg.(value & opt int 8080 & info ["p";"port"] ~docv:"PORT" ~doc)
  in
  let ssl_cert =
    let doc = "SSL certificate file." in
    Arg.(value & opt (some file) None & info ["c";"cert-file"] ~docv:"SSL_CERT" ~doc)
  in
  let ssl_key =
    let doc = "SSL key file" in
    Arg.(value & opt (some file) None & info ["k";"key-file"] ~docv:"SSL_KEY" ~doc)
  in
  let choose host port ssl_cert ssl_key =
    let mode =
      match ssl_cert, ssl_key with
      | Some c, Some k -> `TLS (`Crt_file_path c, `Key_file_path k, `No_password, `Port port)
      | _ -> `TCP (`Port port)
    in
    (host, mode)
  in
  Term.(const choose $ host $ port $ ssl_cert $ ssl_key)
