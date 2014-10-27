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
open Printf

type t = {
  xs: OS.Xs.client;
  name: string
}

type port = string
type uuid = string
type flow = Vchan_xen.t
 
let get_my_id xs =
  OS.Xs.(immediate xs (fun h -> read h "domid"))

let xenstore_register xs myname =
  get_my_id xs >>= fun domid ->
  OS.Xs.(immediate xs (fun h -> write h ("/conduit/" ^ myname) domid))

let get_peer_id xs name =
  Lwt.catch
   (fun () -> OS.Xs.(immediate xs (fun h -> read h ("/conduit/" ^ name))))
   (fun _ -> fail (Failure (sprintf "Conduit_xenstore: %s peer not found" name)))

let readdir h d =
  OS.Xs.(directory h d) >>= fun dirs ->
  let dirs = List.filter (fun p -> p <> "") dirs in
  match dirs with
  | [] -> print_endline "readdir restarting"; fail Xs_protocol.Eagain
  | hd::_ -> print_endline ("readdir returning " ^ hd); return hd

let register name =
  OS.Xs.make () >>= fun xs ->
  (* Check that a /conduit directory exists *)
  catch (fun () ->
    OS.Xs.(immediate xs (fun h -> read h "/conduit"))
    >>= fun _ -> return_unit)
    (fun _ -> fail (Failure
      "No /conduit Xenstore entry found. Run `xenstore-conduit-init`"))
  >>= fun () ->
  xenstore_register xs name >>= fun () ->
  return { xs; name }
 
let accept {xs; name } =
  let waitfn h =
    readdir h (sprintf "/conduit/%s" name) >>= fun remote_name ->
    readdir h (sprintf "/conduit/%s/%s" name remote_name) >>= fun port ->
    OS.Xs.read h (sprintf "/conduit/%s" remote_name) >>= fun remote_domid ->
    let remote_domid = int_of_string remote_domid in
    return (`Vchan (remote_domid, port))
  in
  OS.Xs.wait xs waitfn

let connect {xs; name} ~remote_name ~port =
  get_peer_id xs remote_name
  >>= fun remote_domid ->
  let remote_domid = int_of_string remote_domid in
  OS.Xs.(immediate xs (fun h -> write h
     (sprintf "/conduit/%s/%s/%s" remote_name name port) port))
  >>= fun () ->
  return (`Vchan (remote_domid, port))
