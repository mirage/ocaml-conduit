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

open Sexplib.Std

type service = {
  name: string;
  port: int;
  tls: bool
} with sexp

module Make(IO:Conduit.IO) = struct
  open IO

  type svc = service
  type 'a io = 'a IO.t

  (** A rewrite modifies an input URI with more specialization
      towards a concrete [endp] *)
  type rewrite_fn = service -> Uri.t -> Conduit.endp IO.t with sexp
  type service_fn = string -> service option IO.t with sexp

  type t = {
    default_lookup : rewrite_fn;
    mutable domains: rewrite_fn Conduit_trie.t;
    mutable service: service_fn;
  } with sexp

  let default_lookup _ uri =
    (* TODO log *)
    let host =
      match Uri.host uri with
      | None -> ""
      | Some host -> host
    in
    return (`Unknown host)

  let default_service _name =
    (* TODO log *)
    return None

  let host_to_domain_list host =
    (* TODO: slow, specialise the Trie to be a rev string list instead *)
    String.concat "." (List.rev (Stringext.split ~on:'.' host))

  let add_rewrite ~host ~f t =
    t.domains <- Conduit_trie.insert (host_to_domain_list host) f t.domains

  let set_service ~f t =
    t.service <- f

  let init ?(service=default_service) ?(rewrites=[]) () =
    let domains = Conduit_trie.empty in
    let t = { domains; default_lookup; service } in
    List.iter (fun (host,f) -> add_rewrite ~host ~f t) rewrites;
    t

  let resolve_uri ?rewrites ~uri t =
    (* Find the service associated with the URI *)
    match Uri.scheme uri with
    | None ->
      return (`Unknown "no scheme")
    | Some scheme -> begin
        t.service scheme
        >>= function
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
            match Conduit_trie.longest_prefix (host_to_domain_list host) trie with
            | None -> t.default_lookup
            | Some fn -> fn
          in
          fn service uri
          >>= fun endp ->
          if service.tls then
            return (`TLS (host, endp))
          else
            return endp
      end
end
