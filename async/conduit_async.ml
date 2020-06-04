module Async_scheduler = struct
  type +'a t = 'a Async.Deferred.t

  let bind x f = Async.Deferred.bind x ~f

  let return x = Async.Deferred.return x
end

include Conduit.Make (Async_scheduler) (Cstruct) (Cstruct)

let failwith fmt = Format.kasprintf failwith fmt

let ( >>? ) x f = Async.Deferred.Result.bind x ~f

let serve_with_handler :
    type cfg master flow.
    handler:(flow Service.protocol -> flow -> unit Async.Deferred.t) ->
    service:(cfg, master * flow) Service.service ->
    cfg ->
    unit Async.Condition.t * unit Async.Deferred.t =
 fun ~handler ~service cfg ->
  let open Async in
  let stop = Async.Condition.create () in
  let module Svc = (val Service.impl service) in
  let main =
    Service.serve cfg ~service >>= function
    | Error err -> failwith "%a" Service.pp_error err
    | Ok (master, protocol) -> (
        let rec loop () =
          let close = Async.Condition.wait stop >>| fun () -> Ok `Stop in
          let accept =
            Svc.accept master >>? fun flow ->
            Async.(Deferred.ok (return (`Flow flow))) in

          Async.Deferred.any [ close; accept ] >>= function
          | Ok (`Flow flow) ->
              Async.don't_wait_for (handler protocol flow) ;
              Async.Scheduler.yield () >>= fun () -> (loop [@tailcall]) ()
          | Ok `Stop -> Svc.close master
          | Error err0 -> (
              Svc.close master >>= function
              | Ok () -> Async.return (Error err0)
              | Error _err1 -> Async.return (Error err0)) in
        loop () >>= function
        | Ok () -> Async.return ()
        | Error err -> failwith "%a" Svc.pp_error err) in
  (stop, main)

let reader_and_writer_of_flow flow =
  let open Async in
  let recv flow writer =
    let tmp = Cstruct.create 0x1000 in
    let rec loop () =
      Client.recv flow tmp >>= function
      | Ok (`Input len) ->
          Pipe.write writer (Cstruct.to_string (Cstruct.sub tmp 0 len)) >>= loop
      | Ok `End_of_input ->
          Pipe.close writer ;
          Async.return ()
      | Error err -> failwith "%a" Client.pp_error err in
    loop () in
  let send flow reader =
    let rec loop () =
      Pipe.read reader >>= function
      | `Eof -> Async.return ()
      | `Ok v ->
          let rec go tmp =
            if Cstruct.len tmp = 0
            then Async.return ()
            else
              Client.send flow tmp >>= function
              | Ok shift -> go (Cstruct.shift tmp shift)
              | Error err -> failwith "%a" Client.pp_error err in
          go (Cstruct.of_string v) >>= loop in
    loop () in
  let preader = Pipe.create_reader ~close_on_exception:true (recv flow) in
  let pwriter = Pipe.create_writer (send flow) in
  Reader.of_pipe (Core.Info.of_string "reader") preader >>= fun reader ->
  Writer.of_pipe (Core.Info.of_string "writer") pwriter >>= fun (writer, _) ->
  Async.return (reader, writer)
