open Lwt.Infix

let ( >>? ) x f =
  x >>= function Ok x -> f x | Error _ as err -> Lwt.return err

let reword_error f = function Ok _ as v -> v | Error err -> Error (f err)

type ('edn, 'flow) endpoint = {
  context : Ssl.context;
  endpoint : 'edn;
  verify :
    Ssl.context -> 'flow -> (Lwt_ssl.socket, [ `Verify of string ]) result Lwt.t;
}

let endpoint ~file_descr ~context ?verify endpoint =
  let verify =
    match verify with
    | Some verify -> verify
    | None ->
        let verify ctx flow =
          let file_descr = file_descr flow in
          Lwt_ssl.ssl_connect file_descr ctx >>= fun v -> Lwt.return_ok v in
        verify in
  { context; endpoint; verify }

let pf = Format.fprintf

module Flow (Flow : Conduit_lwt.FLOW) = struct
  type input = Cstruct.t

  type output = Cstruct.t

  type +'a io = 'a Lwt.t

  type error = [ `Error of Flow.error | `Verify of string ]

  let error x = reword_error (fun e -> `Error e) x

  let pp_error ppf = function
    | `Error err -> Flow.pp_error ppf err
    | `Verify err -> pf ppf "%s" err

  type flow = Lwt_ssl.socket

  let recv socket raw =
    let { Cstruct.buffer; off; len } = raw in
    Lwt_ssl.read_bytes socket buffer off len >>= function
    | 0 -> Lwt.return_ok `End_of_flow
    | len -> Lwt.return_ok (`Input len)

  let send socket raw =
    let { Cstruct.buffer; off; len } = raw in
    Lwt_ssl.write_bytes socket buffer off len >>= fun len -> Lwt.return_ok len

  let close socket =
    Lwt_ssl.ssl_shutdown socket >>= fun () ->
    Lwt_ssl.close socket >>= fun () -> Lwt.return_ok ()
end

module Protocol (Protocol : Conduit_lwt.PROTOCOL) = struct
  include Flow (Protocol)

  type nonrec endpoint = (Protocol.endpoint, Protocol.flow) endpoint

  let connect { context; endpoint; verify } =
    Protocol.connect endpoint >|= error >>? fun flow ->
    verify context flow >|= function
    | Ok _ as v -> v
    | Error (`Verify _ as err) -> Error err
end

let protocol_with_ssl :
    type edn flow.
    (edn, flow) Conduit_lwt.protocol ->
    ((edn, flow) endpoint, Lwt_ssl.socket) Conduit_lwt.protocol =
 fun protocol ->
  let module Flow = (val Conduit_lwt.impl protocol) in
  let module M = Protocol (Flow) in
  Conduit_lwt.register ~protocol:(module M)

type 't service = { service : 't; context : Ssl.context }

module Service (Service : sig
  include Conduit_lwt.SERVICE

  val file_descr : flow -> Lwt_unix.file_descr
end) =
struct
  include Flow (Service)

  type configuration = Ssl.context * Service.configuration

  type t = Service.t service

  let init (context, edn) =
    Service.init edn >|= error >>? fun service ->
    Lwt.return_ok { service; context }

  let accept { service; context } =
    Service.accept service >|= error >>? fun flow ->
    let accept () = Lwt_ssl.ssl_accept (Service.file_descr flow) context in
    let process socket = Lwt.return_ok socket in
    let error exn =
      Lwt_unix.close (Service.file_descr flow) >>= fun () -> Lwt.fail exn in
    Lwt.try_bind accept process error

  let stop { service; _ } = Service.stop service >|= error
end

let service_with_ssl :
    type cfg t flow.
    (cfg, t, flow) Conduit_lwt.Service.service ->
    file_descr:(flow -> Lwt_unix.file_descr) ->
    (Ssl.context * cfg, t service, Lwt_ssl.socket) Conduit_lwt.Service.service =
 fun service ~file_descr ->
  let module S = (val Conduit_lwt.Service.impl service) in
  let module M = Service (struct
    include S

    let file_descr = file_descr
  end) in
  Conduit_lwt.Service.register ~service:(module M)

module TCP = struct
  let resolve ~port ~context ?verify domain_name =
    let file_descr = Conduit_lwt.TCP.Protocol.file_descr in
    Conduit_lwt.TCP.resolve ~port domain_name >|= function
    | Some edn -> Some (endpoint ~context ~file_descr ?verify edn)
    | None -> None

  open Conduit_lwt.TCP

  type verify =
    Ssl.context ->
    Protocol.flow ->
    (Lwt_ssl.socket, [ `Verify of string ]) result Lwt.t

  let protocol = protocol_with_ssl protocol

  let service = service_with_ssl service ~file_descr:Protocol.file_descr

  include (val Conduit_lwt.repr protocol)
end
