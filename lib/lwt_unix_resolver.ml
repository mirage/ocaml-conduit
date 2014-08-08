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

let is_tls_service =
  (* TODO fill in the blanks. nowhere else to get this information *)
  function
  | "https" | "imaps" -> true
  | _ -> false

let system_service name =
  (* TODO memoize *)
  try_lwt 
    Lwt_unix.getservbyname name "tcp"
    >>= fun s ->
    let tls = is_tls_service name in
    let svc = { Conduit_resolver.name; port=s.Lwt_unix.s_port; tls } in
    return (Some svc)
  with Not_found ->
    return None

let get_host uri =
  match Uri.host uri with
  | None -> "localhost"
  | Some host -> host

let get_port service uri =
  match Uri.port uri with
  | None -> service.Conduit_resolver.port
  | Some port -> port
 
(* Build a default resolver that uses the system gethostbyname and
   the /etc/services file *)
let system_resolver service uri =
  let open Lwt_unix in
  let host = get_host uri in
  let port = get_port service uri in
  getaddrinfo host (string_of_int port) [AI_SOCKTYPE SOCK_STREAM]
  >>= function
  | [] -> return (`Unknown ("name resolution failed"))
  | {ai_addr=ADDR_INET (addr,port);_}::_ ->
      return (`TCP (Ipaddr_unix.of_inet_addr addr, port))
  | {ai_addr=ADDR_UNIX file;_}::_ ->
      return (`Unix_domain_socket file)

let static_resolver hosts _service uri =
  try
    return (Hashtbl.find hosts (get_host uri))
  with Not_found -> 
    return (`Unknown ("name resolution failed"))
    
let system =
  let service = system_service in
  let rewrites = ["", system_resolver] in
  Lwt_conduit_resolver.init ~service ~rewrites ()

(* Build a default resolver from a static set of lookup rules *)
let static hosts =
  let service = system_service in
  let rewrites = ["", static_resolver hosts] in
  Lwt_conduit_resolver.init ~service ~rewrites ()
