module IO = struct
  type +'a t = 'a Lwt.t

  let bind x f = Lwt.bind x f

  let return x = Lwt.return x
end

include Conduit.Make (IO) (Cstruct) (Cstruct)

let failwith fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

let io_of_flow flow =
  let open Lwt.Infix in
  let ic_closed = ref false and oc_closed = ref false in
  let close () =
    if !ic_closed && !oc_closed
    then
      close flow >>= function
      | Ok () -> Lwt.return_unit
      | Error err -> failwith "%a" pp_error err
    else Lwt.return_unit in
  let ic_close () =
    ic_closed := true ;
    close () in
  let oc_close () =
    oc_closed := true ;
    close () in
  let recv buf off len =
    let raw = Cstruct.of_bigarray buf ~off ~len in
    recv flow raw >>= function
    | Ok (`Input len) -> Lwt.return len
    | Ok `End_of_flow -> Lwt.return 0
    | Error err -> failwith "%a" pp_error err in
  let ic = Lwt_io.make ~close:ic_close ~mode:Lwt_io.input recv in
  let send buf off len =
    let raw = Cstruct.of_bigarray buf ~off ~len in
    send flow raw >>= function
    | Ok len -> Lwt.return len
    | Error err -> failwith "%a" pp_error err in
  let oc = Lwt_io.make ~close:oc_close ~mode:Lwt_io.output send in
  (ic, oc)

let ( >>? ) = Lwt_result.bind

let serve :
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
    Service.init cfg ~service >>= function
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

  val protocol : (endpoint, flow) protocol

  val service : (configuration, master, flow) Service.service
end
