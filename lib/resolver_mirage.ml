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
  | None -> service.Resolver.port
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
     let svc = { Resolver.name; port; tls } in
     return (Some svc)

let static hosts =
  let service = static_service in
  let rewrites = ["", static_resolver hosts] in
  Resolver_lwt.init ~service ~rewrites ()

let localhost =
  let hosts = Hashtbl.create 3 in
  Hashtbl.add hosts "localhost"
              (fun ~port -> `TCP (Ipaddr.(V4 V4.localhost), port));
  static hosts


module type S = sig
  module DNS : Dns_resolver_mirage.S
  val default_ns : Ipaddr.V4.t
  val vchan_resolver : tld:string -> Resolver_lwt.rewrite_fn
  val dns_stub_resolver:
    ?ns:Ipaddr.V4.t -> ?ns_port:int -> DNS.t -> Resolver_lwt.rewrite_fn
  val register:
    ?ns:Ipaddr.V4.t -> ?ns_port:int -> ?stack:DNS.stack ->
    Resolver_lwt.t -> unit
  val init:
    ?ns:Ipaddr.V4.t -> ?ns_port:int -> ?stack:DNS.stack -> unit -> Resolver_lwt.t
end

module Make(DNS:Dns_resolver_mirage.S) = struct
  module DNS = DNS

  type t = {
    dns: DNS.t;
    ns: Ipaddr.V4.t;
    dns_port: int;
  }

  let vchan_resolver ~tld =
    let tld_len = String.length tld in
    let get_short_host uri =
      let n = get_host uri in
      let len = String.length n in
      if len > tld_len && (String.sub n (len-tld_len) tld_len = tld) then
        String.sub n 0 (len-tld_len)
      else
        n
    in
    fun service uri ->
      (* Strip the tld from the hostname *)
      let remote_name = get_short_host uri in
      Printf.printf "vchan_lookup: %s %s -> normalizes to %s\n%!"
        (Sexplib.Sexp.to_string_hum (Resolver.sexp_of_service service))
        (Uri.to_string uri) remote_name;
      return (`Vchan_domain_socket (remote_name, service.Resolver.name))

  let default_ns = Ipaddr.V4.of_string_exn "8.8.8.8"

  let dns_stub_resolver ?(ns=default_ns) ?(ns_port=53) dns service uri
      : Conduit.endp Lwt.t =
    let host = get_host uri in
    let port = get_port service uri in
    (match Ipaddr.of_string host with
    | None -> DNS.gethostbyname ~server:ns ~dns_port:ns_port dns host
    | Some addr -> return [addr]) >>= fun res ->
    List.filter (function Ipaddr.V4 _ -> true | _ -> false) res
    |> function
    | [] -> return (`Unknown ("name resolution failed"))
    | addr::_ -> return (`TCP (addr,port))

  let register ?(ns=default_ns) ?(ns_port=53) ?stack res =
      begin match stack with
      | Some s ->
         (* DNS stub resolver *)
         let dns = DNS.create s in
         let f = dns_stub_resolver ~ns ~ns_port dns in
         Resolver_lwt.add_rewrite ~host:"" ~f res
      | None -> ()
      end;
      let service = Resolver_lwt.(service res ++ static_service) in
      Resolver_lwt.set_service ~f:service res;
      let vchan_tld = ".xen" in
      let vchan_res = vchan_resolver ~tld:vchan_tld in
      Resolver_lwt.add_rewrite ~host:vchan_tld ~f:vchan_res res

  let init ?ns ?ns_port ?stack () =
    let res = Resolver_lwt.init () in
    register ?ns ?ns_port ?stack res;
    res
end

module Make_with_stack (T: V1_LWT.TIME) (S: V1_LWT.STACKV4) = struct
  module R = Make(Dns_resolver_mirage.Make(T)(S))
  include Resolver_lwt
end
