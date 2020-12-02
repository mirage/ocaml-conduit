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
    (cfg, service, flow) Service.t ->
    cfg ->
    unit Lwt_condition.t * unit Lwt.t =
 fun ~handler service cfg ->
  let open Lwt.Infix in
  let stop = Lwt_condition.create () in
  let module Svc = (val Service.impl service) in
  let main =
    Service.init service cfg >>= function
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
          | Ok `Stop -> Svc.stop service
          | Error err0 -> (
              Svc.stop service >>= function
              | Ok () -> Lwt.return_error err0
              | Error _err1 -> Lwt.return_error err0) in
        loop () >>= function
        | Ok () -> Lwt.return_unit
        | Error err -> failwith "%a" Svc.pp_error err) in
  (stop, main)

open Lwt.Infix

let failwithf fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

module type Mirage_protocol = sig
  include Mirage_flow.S

  type endpoint

  val connect : endpoint -> (flow, error) result Lwt.t
end

let blit0 :
    (char, Bigarray.int8_unsigned_elt) Ke.Rke.N.bigarray ->
    int ->
    Cstruct.t ->
    int ->
    int ->
    unit =
 fun src src_off dst dst_off len' ->
  Cstruct.(
    blit
      (of_bigarray ~off:src_off ~len:len' src)
      0 (sub dst dst_off len') 0 len')

let blit1 :
    Cstruct.t ->
    int ->
    (char, Bigarray.int8_unsigned_elt) Ke.Rke.N.bigarray ->
    int ->
    int ->
    unit =
 fun src src_off dst dst_off len' ->
  Cstruct.(
    blit (sub src src_off len') 0
      (of_bigarray ~off:dst_off ~len:len' dst)
      0 len')

let page_size = 4096

type 'flow flow0 = {
  flow : 'flow;
  queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
}

type 'flow flow1 = {
  flow : 'flow;
  linger : Cstruct.t;
  queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
}

module Make0 (Protocol : Mirage_protocol) = struct
  type +'a io = 'a Lwt.t

  type input = Cstruct.t

  and output = Cstruct.t

  type flow = Protocol.flow flow0

  type endpoint = Protocol.endpoint

  type error = [ `Error of Protocol.error | `Write of Protocol.write_error ]

  let pp_error ppf = function
    | `Error err -> Protocol.pp_error ppf err
    | `Write err -> Protocol.pp_write_error ppf err

  let connect edn =
    Protocol.connect edn >>= function
    | Ok flow ->
        Lwt.return
          (Ok
             {
               flow;
               queue = Ke.Rke.create ~capacity:(page_size * 2) Bigarray.char;
             })
    | Error err -> Lwt.return (Error (`Error err))

  let recv ({ flow; queue } : flow) payload =
    if Ke.Rke.length queue > 0
    then (
      let len = Cstruct.len payload in
      Ke.Rke.N.keep_exn queue ~blit:blit0 ~length:Cstruct.len ~off:0 ~len
        payload ;
      Ke.Rke.N.unsafe_shift queue len ;
      Lwt.return (Ok (`Input len)))
    else
      Protocol.read flow >>= function
      | Ok (`Data v) ->
          Ke.Rke.N.push queue ~blit:blit1 ~length:Cstruct.len ~off:0
            ~len:(Cstruct.len v) v ;
          let len = min (Cstruct.len payload) (Ke.Rke.length queue) in
          Ke.Rke.N.keep_exn queue ~blit:blit0 ~length:Cstruct.len ~off:0 ~len
            payload ;
          Ke.Rke.N.unsafe_shift queue len ;
          Lwt.return (Ok (`Input len))
      | Ok `Eof -> Lwt.return (Ok `End_of_flow)
      | Error err -> Lwt.return (Error (`Error err))

  let send ({ flow; _ } : flow) payload =
    let len = Cstruct.len payload in
    Protocol.write flow payload >>= function
    | Ok () -> Lwt.return (Ok len)
    | Error err -> Lwt.return (Error (`Write err))

  let close ({ flow; _ } : flow) =
    Protocol.close flow >>= fun () -> Lwt.return (Ok ())
end

let protocol_of_mirage_flow :
    type flow edn.
    (module Mirage_protocol with type flow = flow and type endpoint = edn) ->
    (edn, flow flow0) protocol =
 fun (module Mirage_protocol) ->
  let module Protocol = Make0 (Mirage_protocol) in
  register (module Protocol)

module Make1
    (Protocol : Conduit.Conduit_intf.PROTOCOL
                  with type 'a io = 'a Lwt.t
                   and type input = Cstruct.t
                   and type output = Cstruct.t) =
struct
  type error = Protocol.error

  type write_error = [ `Write of Protocol.error | `Closed ]

  let pp_error = Protocol.pp_error

  let pp_write_error ppf = function
    | `Write err -> Protocol.pp_error ppf err
    | `Closed -> Format.fprintf ppf "Connection closed"

  type flow = Protocol.flow flow1

  type endpoint = Protocol.endpoint

  let connect edn =
    Protocol.connect edn >>= function
    | Ok flow ->
        Lwt.return
          (Ok
             {
               flow;
               linger = Cstruct.create page_size;
               queue = Ke.Rke.create ~capacity:(page_size * 2) Bigarray.char;
             })
    | Error err -> failwithf "%a" Protocol.pp_error err

  let read { flow; linger; _ } =
    Protocol.recv flow linger >>= function
    | Ok (`Input len) -> Lwt.return (Ok (`Data (Cstruct.sub linger 0 len)))
    | Ok `End_of_flow -> Lwt.return (Ok `Eof)
    | Error _ as err -> Lwt.return err

  let write { flow; queue; _ } payload =
    let len = Cstruct.len payload in
    Ke.Rke.N.push queue ~blit:blit1 ~length:Cstruct.len ~off:0 ~len payload ;
    match Ke.Rke.N.peek queue with
    | [] -> Lwt.return (Ok ())
    | hd :: _ -> (
        let len = Bigarray.Array1.dim hd in
        Protocol.send flow (Cstruct.of_bigarray hd ~off:0 ~len) >>= function
        | Ok len ->
            Ke.Rke.N.unsafe_shift queue len ;
            Lwt.return (Ok ())
        | Error err -> Lwt.return (Error (`Write err)))

  let writev t ps =
    let rec go = function
      | [] -> Lwt.return (Ok ())
      | hd :: tl -> (
          write t hd >>= function
          | Ok () -> go tl
          | Error _ as err -> Lwt.return err) in
    go ps

  let close { flow; _ } =
    Protocol.close flow >>= function
    | Ok () -> Lwt.return ()
    | Error err -> failwithf "%a" Protocol.pp_error err
end
