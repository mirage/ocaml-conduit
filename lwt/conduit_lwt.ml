module Lwt_scheduler = struct
  type +'a t = 'a Lwt.t

  let bind x f = Lwt.bind x f

  let return x = Lwt.return x
end

include Conduit.Make (Lwt_scheduler) (Cstruct) (Cstruct)

let failwith fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

let ( >>? ) = Lwt_result.bind

let serve_with_handler :
    type cfg master flow.
    handler:(flow -> unit Lwt.t) ->
    service:(cfg, master, flow) Service.service ->
    cfg ->
    unit Lwt_condition.t * unit Lwt.t =
 fun ~handler ~service cfg ->
  let open Lwt.Infix in
  let stop = Lwt_condition.create () in
  let module Svc = (val Service.impl service) in
  let main =
    Service.serve cfg ~service >>= function
    | Error err -> failwith "%a" Service.pp_error err
    | Ok master -> (
        let rec loop () =
          let stop = Lwt_condition.wait stop >>= fun () -> Lwt.return_ok `Stop in
          let accept =
            Svc.accept master >>? fun flow -> Lwt.return_ok (`Flow flow) in

          Lwt.pick [ stop; accept ] >>= function
          | Ok (`Flow flow) ->
              Lwt.async (fun () -> handler flow) ;
              Lwt.pause () >>= loop
          | Ok `Stop -> Svc.close master
          | Error err0 -> (
              Svc.close master >>= function
              | Ok () -> Lwt.return_error err0
              | Error _err1 -> Lwt.return_error err0) in
        loop () >>= function
        | Ok () -> Lwt.return_unit
        | Error err -> failwith "%a" Svc.pp_error err) in
  (stop, main)

module type CONDUIT = sig
  type endpoint

  type flow

  type configuration

  type master

  val protocol : (endpoint, flow) Client.protocol

  val service : (configuration, master, flow) Service.service
end
