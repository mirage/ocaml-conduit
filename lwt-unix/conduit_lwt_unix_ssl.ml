open Lwt.Infix

let ( >>? ) x f =
  x >>= function Ok x -> f x | Error _ as err -> Lwt.return err

let reword_error f = function Ok _ as v -> v | Error err -> Error (f err)

let failwith fmt = Format.kasprintf (fun err -> raise (Failure err)) fmt

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

module Protocol (Flow : Conduit_lwt_unix.PROTOCOL) = struct
  type input = Cstruct.t

  type output = Cstruct.t

  type +'a s = 'a Lwt.t

  type error = [ `Flow of Flow.error | `Verify of string ]

  let pp_error ppf = function
    | `Flow err -> Flow.pp_error ppf err
    | `Verify err -> pf ppf "%s" err

  type flow = Lwt_ssl.socket

  type nonrec endpoint = (Flow.endpoint, Flow.flow) endpoint

  let flow { context; endpoint; verify } =
    Flow.flow endpoint >|= reword_error (fun err -> `Flow err) >>? fun flow ->
    verify context flow >>= function
    | Ok _ as v -> Lwt.return v
    | Error (`Verify _ as err) -> Lwt.return (Error err)

  let recv socket raw =
    let { Cstruct.buffer; off; len } = raw in
    Lwt_ssl.read_bytes socket buffer off len >>= function
    | 0 -> Lwt.return_ok `End_of_input
    | len -> Lwt.return_ok (`Input len)

  let send socket raw =
    let { Cstruct.buffer; off; len } = raw in
    Lwt_ssl.write_bytes socket buffer off len >>= fun len -> Lwt.return_ok len

  let close socket =
    Lwt_ssl.ssl_shutdown socket >>= fun () ->
    Lwt_ssl.close socket >>= fun () -> Lwt.return_ok ()
end

let protocol_with_ssl :
    type edn flow.
    key:edn Conduit_lwt_unix.key ->
    flow Conduit_lwt_unix.Witness.protocol ->
    (edn, flow) endpoint Conduit_lwt_unix.key
    * Lwt_ssl.socket Conduit_lwt_unix.Witness.protocol =
 fun ~key protocol ->
  match Conduit_lwt_unix.impl_of_protocol ~key protocol with
  | Ok (module Flow) ->
      let module M = Protocol (Flow) in
      let k =
        Conduit_lwt_unix.key
          (Fmt.strf "%s + ssl" (Conduit_lwt_unix.name_of_key key)) in
      let p = Conduit_lwt_unix.register_protocol ~key:k ~protocol:(module M) in
      (k, p)
  | Error _ ->
      failwith "Invalid key %s with given protocol"
        (Conduit_lwt_unix.name_of_key key)

type 't master = { master : 't; context : Ssl.context }

module Service (Service : sig
  include Conduit_lwt_unix.SERVICE

  val file_descr : flow -> Lwt_unix.file_descr
end) =
struct
  type +'a s = 'a Lwt.t

  type endpoint = Ssl.context * Service.endpoint

  type t = Service.t master

  type flow = Lwt_ssl.socket

  type error = [ `Service of Service.error ]

  let pp_error ppf (`Service err) = Service.pp_error ppf err

  let make (context, edn) =
    Service.make edn >|= reword_error (fun err -> `Service err)
    >>? fun master -> Lwt.return_ok { master; context }

  let accept { master; context } =
    Service.accept master >|= reword_error (fun err -> `Service err)
    >>? fun flow ->
    let accept () = Lwt_ssl.ssl_accept (Service.file_descr flow) context in
    let process socket = Lwt.return_ok socket in
    let error exn =
      Lwt_unix.close (Service.file_descr flow) >>= fun () -> Lwt.fail exn in
    Lwt.try_bind accept process error

  let close { master; _ } =
    Service.close master >|= reword_error (fun err -> `Service err)
end

let service_with_ssl :
    type edn t flow.
    key:edn Conduit_lwt_unix.key ->
    (t * flow) Conduit_lwt_unix.Witness.service ->
    file_descr:(flow -> Lwt_unix.file_descr) ->
    Lwt_ssl.socket Conduit_lwt_unix.Witness.protocol ->
    (Ssl.context * edn) Conduit_lwt_unix.key
    * (t master * Lwt_ssl.socket) Conduit_lwt_unix.Witness.service =
 fun ~key service ~file_descr protocol ->
  match Conduit_lwt_unix.impl_of_service ~key service with
  | Ok (module S) ->
      let module M = Service (struct
        include S

        let file_descr = file_descr
      end) in
      let k =
        Conduit_lwt_unix.key
          (Fmt.strf "%s + ssl" (Conduit_lwt_unix.name_of_key key)) in
      let s =
        Conduit_lwt_unix.register_service ~key:k ~service:(module M) ~protocol
      in
      (k, s)
  | Error _ ->
      failwith "Invalid key %s with given service"
        (Conduit_lwt_unix.name_of_key key)

module TCP = struct
  let resolv_conf ~port ~context ?verify domain_name =
    let file_descr = Conduit_lwt_unix_tcp.Protocol.file_descr in
    Conduit_lwt_unix_tcp.resolv_conf ~port domain_name >|= function
    | Some edn -> Some (endpoint ~context ~file_descr ?verify edn)
    | None -> None

  open Conduit_lwt_unix_tcp

  type verify =
    Ssl.context ->
    Protocol.flow ->
    (Lwt_ssl.socket, [ `Verify of string ]) result Lwt.t

  let endpoint, protocol = protocol_with_ssl ~key:endpoint protocol

  let configuration, service =
    service_with_ssl ~key:configuration service ~file_descr:Protocol.file_descr
      protocol
end
