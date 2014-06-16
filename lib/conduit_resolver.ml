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
open Sexplib.Std

(** The resolver will return an [endp], which the Conduit
    backend must interpret to make a connection. *)
type endp = [
  | `TCP of Ipaddr.t * int        (** ipaddr and dst port *)
  | `Unix_domain_socket of string (** unix file path *)
  | `Vchan of string list         (** xenstore path *)
  | `TLS of endp                  (** wrap in a TLS channel *)
  | `Unknown of string            (** failed resolution *)
] with sexp

type service = {
  name: string;
  port: int;
  tls: bool
} with sexp

(** A rewrite modifies an input URI with more specialization
    towards a concrete [endp] *)
type fn = service -> Uri.t -> endp Lwt.t with sexp

type t = {
  default_lookup : fn;
  mutable domains: fn Conduit_trie.t;
  service: (string, service) Hashtbl.t;
} with sexp

let default_lookup _ uri =
  let host =
    match Uri.host uri with
    | None -> ""
    | Some host -> host
  in
  return (`Unknown host)

let init () =
  let service = Hashtbl.create 7 in
  let domains = Conduit_trie.empty in
  { domains; default_lookup; service }

let host_to_domain_list host =
  (* TODO: slow, specialise the Trie to be a rev string list instead *)
  String.concat "." (List.rev (Stringext.split ~on:'.' host))

let add_domain_rewrite ~host ~f t =
  t.domains <- Conduit_trie.insert (host_to_domain_list host) f t.domains

let add_service ~name ~service t =
  Hashtbl.add t.service name service

let remove_service ~name t =
  Hashtbl.remove t.service name

let find_service ~name t =
  match Hashtbl.mem t.service name with
  | false -> None
  | true -> Some (Hashtbl.find t.service name)

let resolve_uri ?rewrites ~uri t =
  (* Find the service associated with the URI *)
  match Uri.scheme uri with
  | None ->
     return (`Unknown "no scheme")
  | Some scheme -> begin
     match find_service ~name:scheme t with
     | None -> return (`Unknown "unknown scheme")
     | Some service ->
        let host =
          match Uri.host uri with
          | None -> "localhost"
          | Some host -> host
        in
        let trie =
          (* If there are local rewrites, add them to the trie *)
          match rewrites with
          | None -> t.domains
          | Some rewrites ->
              List.fold_left (fun acc (host, f) -> 
                Conduit_trie.insert (host_to_domain_list host) f acc)
                t.domains rewrites
        in
        (* Find the longest prefix function that matches this host *)
        let fn =
          match Conduit_trie.longest_prefix host trie with
          | None -> t.default_lookup
          | Some fn -> fn
        in
        fn service uri
        >>= fun endp ->
        if service.tls then
          return (`TLS endp)
        else
          return endp
  end
