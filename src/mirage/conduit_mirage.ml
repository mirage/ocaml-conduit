module IO = struct
  type +'a t = 'a Lwt.t

  let bind x f = Lwt.bind x f

  let return x = Lwt.return x
end

include Conduit.Make (IO) (Cstruct) (Cstruct)

let failwith fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

let ( >>? ) = Lwt_result.bind

let serve :
    type cfg service flow.
    handler:(flow -> unit Lwt.t) ->
    service:(cfg, service, flow) Service.service ->
    cfg ->
    unit Lwt_condition.t * unit Lwt.t =
 fun ~handler ~service cfg ->
  let open Lwt.Infix in
  let stop = Lwt_condition.create () in
  let module Svc = (val Service.impl service) in
  let main =
    Service.init cfg ~service >>= function
    | Error err -> failwith "%a" Service.pp_error err
    | Ok service -> (
        let rec loop () =
          let stop = Lwt_condition.wait stop >>= fun () -> Lwt.return_ok `Stop in
          let accept =
            Svc.accept service >>? fun flow -> Lwt.return_ok (`Flow flow) in

          Lwt.pick [ stop; accept ] >>= function
          | Ok (`Flow flow) ->
              Lwt.async (fun () -> handler flow) ;
              Lwt.pause () >>= loop
          | Ok `Stop -> Svc.close service
          | Error err0 -> (
              Svc.close service >>= function
              | Ok () -> Lwt.return_error err0
              | Error _err1 -> Lwt.return_error err0) in
        loop () >>= function
        | Ok () -> Lwt.return_unit
        | Error err -> failwith "%a" Svc.pp_error err) in
  (stop, main)
