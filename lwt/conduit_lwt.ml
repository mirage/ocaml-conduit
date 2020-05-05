module Lwt_scheduler = struct
  type +'a t = 'a Lwt.t

  let bind x f = Lwt.bind x f

  let return x = Lwt.return x
end

include Conduit.Make (Lwt_scheduler) (Cstruct) (Cstruct)

let failwith fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

let invalid_arg fmt = Format.kasprintf invalid_arg fmt

let ( >>? ) = Lwt_result.bind

let serve_with_handler :
    type cfg master flow.
    handler:(flow Witness.protocol -> flow -> unit Lwt.t) ->
    key:cfg key ->
    service:(master * flow) Witness.service ->
    cfg ->
    unit Lwt_condition.t * unit Lwt.t =
 fun ~handler ~key ~service cfg ->
  let open Lwt.Infix in
  let stop = Lwt_condition.create () in
  match impl_of_service ~key service with
  | Error _ -> invalid_arg "Invalid key %s" (name_of_key key)
  | Ok (module Service) ->
      let main =
        serve ~key cfg ~service >>= function
        | Error err -> failwith "%a" pp_error err
        | Ok (master, protocol) -> (
            let rec loop () =
              let stop =
                Lwt_condition.wait stop >>= fun () -> Lwt.return_ok `Stop in
              let accept =
                Service.accept master >>? fun flow -> Lwt.return_ok (`Flow flow)
              in

              Lwt.pick [ stop; accept ] >>= function
              | Ok (`Flow flow) ->
                  Lwt.async (fun () -> handler protocol flow) ;
                  Lwt.pause () >>= loop
              | Ok `Stop -> Service.close master
              | Error err0 -> (
                  Service.close master >>= function
                  | Ok () -> Lwt.return_error err0
                  | Error _err1 -> Lwt.return_error err0) in
            loop () >>= function
            | Ok () -> Lwt.return_unit
            | Error err -> failwith "%a" Service.pp_error err) in
      (stop, main)
