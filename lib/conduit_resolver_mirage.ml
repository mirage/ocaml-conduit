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

let get_host uri =
  match Uri.host uri with
  | None -> "localhost"
  | Some host -> 
      match Ipaddr.of_string host with
      | Some ip -> Ipaddr.to_string ip
      | None -> host

let get_port service uri =
  match Uri.port uri with
  | None -> service.Conduit_resolver.port
  | Some port -> port

let static_resolver hosts service uri =
  let port = get_port service uri in
  try
    let fn = Hashtbl.find hosts (get_host uri) in
    return (fn ~port)
  with Not_found -> 
    return (`Unknown ("name resolution failed"))

let static_service name =
  match Uri_services.tcp_port_of_service name with
  | [] -> return None
  | port::_ ->
     let tls = is_tls_service name in
     let svc = { Conduit_resolver.name; port; tls } in
     return (Some svc)

let static hosts =
  let service = static_service in
  let rewrites = ["", static_resolver hosts] in
  Conduit_resolver_lwt.init ~service ~rewrites ()

let localhost =
  let hosts = Hashtbl.create 3 in
  Hashtbl.add hosts "localhost" (fun ~port -> `TCP (Ipaddr.(V4 V4.localhost), port));
  static hosts

(* Build a resolver that uses the stub resolver to perform a
   resolution of the hostname *)
module Make(DNS:Dns_resolver_mirage.S) = struct

  type t = {
    dns: DNS.t;
    ns: Ipaddr.V4.t;
    dns_port: int;
  }

  let stub_resolver t service uri =
    let host = get_host uri in
    let port = get_port service uri in
    DNS.gethostbyname ~server:t.ns ~dns_port:t.dns_port t.dns host
    >>= fun res ->
    List.filter (function Ipaddr.V4 _ -> true | _ -> false) res
    |> function
    | [] -> return (`Unknown ("name resolution failed"))
    | addr::_ -> return (`TCP (addr,port))
  
  let default_ns = Ipaddr.V4.of_string_exn "8.8.8.8"
 
  let system ?(ns=default_ns) ?(dns_port=53) stack =
    let dns = DNS.create stack in
    let t = { dns; ns; dns_port } in
    let service = static_service in
    let rewrites = ["", stub_resolver t] in
    Conduit_resolver_lwt.init ~service ~rewrites ()

end

module type PEER = sig
  type t
  type flow
  type uuid
  type port

  val register : uuid -> t Lwt.t
  val accept : t -> flow Lwt.t
  val connect : t -> remote_name:uuid -> port:port -> flow Lwt.t
end
