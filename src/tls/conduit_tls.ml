[@@@warning "-32"]

exception Tls_alert of Tls.Packet.alert_type

exception Tls_failure of Tls.Engine.failure

module type IO = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  val fail : exn -> 'a t
end

module type S0 = sig
  type +'a io

  type file_descr

  val read : file_descr -> Cstruct.t -> int io

  val write : file_descr -> Cstruct.t -> int io

  val write_full : file_descr -> Cstruct.t -> unit io

  val close : file_descr -> unit io

  type endpoint

  type error

  val pp_error : Format.formatter -> error -> unit

  val connect : endpoint -> (file_descr, error) result io
end

module Make
    (IO : IO)
    (Conduit : Conduit.S
                 with type 'a io = 'a IO.t
                  and type input = Cstruct.t
                  and type output = Cstruct.t) =
struct
  let ( >>= ) x f = IO.bind x f

  let return x = IO.return x

  let ( >|= ) x f = x >>= fun x -> return (f x)

  let return_unit = return ()

  let[@inline never] fail exn = IO.fail exn

  let safely th =
    IO.catch (fun () -> th >>= fun _ -> return_unit) (fun _ -> return_unit)

  type 'flow protocol_with_tls = {
    fd : 'flow;
    mutable state : [ `Active of Tls.Engine.state | `Eof | `Error of exn ];
    mutable linger : Cstruct.t option;
    recv_buf : Cstruct.t;
  }

  module Make_drain_handshake (Protocol : S0 with type 'a io = 'a IO.t) : sig
    val drain_handshake :
      Protocol.file_descr protocol_with_tls ->
      Protocol.file_descr protocol_with_tls IO.t

    val read_react :
      Protocol.file_descr protocol_with_tls ->
      [> `Eof | `Ok of Cstruct.t option ] IO.t

    val write_t :
      Protocol.file_descr protocol_with_tls -> Cstruct.t -> unit IO.t
  end = struct
    let read_t, write_t =
      let recording_errors op t cs =
        IO.catch
          (fun () -> op t.fd cs)
          (fun exn ->
            (match t.state with
            | `Error _ | `Eof -> ()
            | `Active _ -> t.state <- `Error exn) ;
            fail exn) in
      (recording_errors Protocol.read, recording_errors Protocol.write_full)

    let when_some f = function None -> return_unit | Some x -> f x

    let rec read_react t =
      let handle tls buf =
        match Tls.Engine.handle_tls tls buf with
        | `Ok (state', `Response resp, `Data data) ->
            let state' =
              match state' with
              | `Ok tls -> `Active tls
              | `Eof -> `Eof
              | `Alert a -> `Error (Tls_alert a) in
            t.state <- state' ;
            safely (resp |> when_some (write_t t)) >|= fun () -> `Ok data
        | `Fail (alert, `Response resp) ->
            t.state <- `Error (Tls_failure alert) ;
            write_t t resp >>= fun () -> read_react t in

      match t.state with
      | `Error e -> fail e
      | `Eof -> return `Eof
      | `Active _ -> (
          read_t t t.recv_buf >>= fun n ->
          match (t.state, n) with
          | `Active _, 0 ->
              t.state <- `Eof ;
              return `Eof
          | `Active tls, n -> handle tls (Cstruct.sub t.recv_buf 0 n)
          | `Error e, _ -> fail e
          | `Eof, _ -> return `Eof)

    (*
     * XXX bad XXX
     * This is a point that should particularly be protected from concurrent r/w.
     * Doing this before a `t` is returned is safe; redoing it during rekeying is
     * not, as the API client already sees the `t` and can mistakenly interleave
     * writes while this is in progress.
     * *)
    let rec drain_handshake t =
      let push_linger t mcs =
        let open Tls.Utils.Cs in
        match (mcs, t.linger) with
        | None, _ -> ()
        | scs, None -> t.linger <- scs
        | Some cs, Some l -> t.linger <- Some (l <+> cs) in
      match t.state with
      | `Active tls when not (Tls.Engine.handshake_in_progress tls) -> return t
      | _ -> (
          read_react t >>= function
          | `Eof -> fail End_of_file
          | `Ok cs ->
              push_linger t cs ;
              drain_handshake t)
  end

  module type PROTOCOL =
    Conduit.PROTOCOL
      with type 'a io = 'a IO.t
       and type input = Cstruct.t
       and type output = Cstruct.t

  module Make_S0_from_PROTOCOL (Protocol : PROTOCOL) = struct
    exception Local_failure of string

    let failwithf fmt =
      Format.kasprintf (fun err -> fail (Local_failure err)) fmt

    type +'a io = 'a IO.t

    type file_descr = Protocol.flow

    let read file_descr buf =
      Protocol.recv file_descr buf >>= function
      | Ok (`Input len) -> return len
      | Ok `End_of_flow -> return 0
      | Error err -> failwithf "%a" Protocol.pp_error err

    let write file_descr buf =
      Protocol.send file_descr buf >>= function
      | Ok len -> return len
      | Error err -> failwithf "%a" Protocol.pp_error err

    let rec write_full file_descr = function
      | cs when Cstruct.len cs = 0 -> return ()
      | cs -> (
          Protocol.send file_descr cs >>= function
          | Ok len -> write_full file_descr (Cstruct.shift cs len)
          | Error err -> failwithf "%a" Protocol.pp_error err)

    let close file_descr =
      Protocol.close file_descr >>= function
      | Ok () -> return ()
      | Error err -> failwithf "%a" Protocol.pp_error err

    type endpoint = Protocol.endpoint

    type error = Protocol.error

    let pp_error = Protocol.pp_error

    let connect = Protocol.connect
  end

  module Make0 (Protocol : S0 with type 'a io = 'a IO.t) = struct
    type t = Protocol.file_descr protocol_with_tls

    include Make_drain_handshake (Protocol)

    let rec read t buf =
      let writeout res =
        let open Cstruct in
        let rlen = len res in
        let n = min (len buf) rlen in
        blit res 0 buf 0 n ;
        t.linger <- (if n < rlen then Some (sub res n (rlen - n)) else None) ;
        return n in

      match t.linger with
      | Some res -> writeout res
      | None -> (
          read_react t >>= function
          | `Eof -> return 0
          | `Ok None -> read t buf
          | `Ok (Some res) -> writeout res)

    let writev t css =
      match t.state with
      | `Error err -> fail err
      | `Eof -> fail @@ Invalid_argument "tls: closed socket"
      | `Active tls ->
      match Tls.Engine.send_application_data tls css with
      | Some (tls, tlsdata) ->
          t.state <- `Active tls ;
          write_t t tlsdata
      | None -> fail @@ Invalid_argument "tls: write: socket not ready"

    let write t cs = writev t [ cs ]

    let reneg ?authenticator ?acceptable_cas ?cert ?(drop = true) t =
      match t.state with
      | `Error err -> fail err
      | `Eof -> fail @@ Invalid_argument "tls: closed socket"
      | `Active tls ->
      match Tls.Engine.reneg ?authenticator ?acceptable_cas ?cert tls with
      | None -> fail @@ Invalid_argument "tls: can't renegotiate"
      | Some (tls', buf) ->
          if drop then t.linger <- None ;
          t.state <- `Active tls' ;
          write_t t buf >>= fun () ->
          drain_handshake t >>= fun _ -> return_unit

    let key_update ?request t =
      match t.state with
      | `Error err -> fail err
      | `Eof -> fail @@ Invalid_argument "tls: closed socket"
      | `Active tls ->
      match Tls.Engine.key_update ?request tls with
      | Error _ -> fail @@ Invalid_argument "tls: can't update key"
      | Ok (tls', buf) ->
          t.state <- `Active tls' ;
          write_t t buf

    let close_tls t =
      match t.state with
      | `Active tls ->
          let _, buf = Tls.Engine.send_close_notify tls in
          t.state <- `Eof ;
          write_t t buf
      | _ -> return_unit

    let close t = safely (close_tls t) >>= fun () -> Protocol.close t.fd

    let client_of_fd config ?host fd =
      let config' =
        match host with
        | None -> config
        | Some host -> Tls.Config.peer config host in
      let t =
        { state = `Eof; fd; linger = None; recv_buf = Cstruct.create 0x1000 }
      in
      let tls, init = Tls.Engine.client config' in
      let t = { t with state = `Active tls } in
      write_t t init >>= fun () -> drain_handshake t

    (*
  let accept conf fd =
    Lwt_unix.accept fd >>= fun (fd', addr) ->
    Lwt.catch (fun () -> server_of_fd conf fd' >|= fun t -> (t, addr))
      (fun exn -> safely (Lwt_unix.close fd') >>= fun () -> fail exn)

  let connect conf (host, port) =
    resolve host (string_of_int port) >>= fun addr ->
    let fd = Lwt_unix.(socket (Unix.domain_of_sockaddr addr) SOCK_STREAM 0) in
    Lwt.catch (fun () -> Lwt_unix.connect fd addr >>= fun () -> client_of_fd conf ~host fd)
      (fun exn -> safely (Lwt_unix.close fd) >>= fun () -> fail exn)
  *)

    let read_bytes t bs off len = read t (Cstruct.of_bigarray ~off ~len bs)

    let write_bytes t bs off len = write t (Cstruct.of_bigarray ~off ~len bs)

    let epoch t =
      match t.state with
      | `Active tls -> (
          match Tls.Engine.epoch tls with
          | `InitialEpoch -> assert false (* can never occur! *)
          | `Epoch data -> `Ok data)
      | `Eof -> `Error
      | `Error _ -> `Error
  end

  type 'edn endpoint = 'edn * Tls.Config.client

  module Make1 (Protocol : sig
    include PROTOCOL

    val host_of_endpoint : endpoint -> string option
  end) =
  struct
    module M0 = Make_S0_from_PROTOCOL (Protocol)
    include Make0 (M0)

    type flow = t

    type nonrec endpoint = Protocol.endpoint endpoint

    type input = Cstruct.t

    and output = Cstruct.t

    type +'a io = 'a IO.t

    type error =
      [ `Tls_alert of Tls.Packet.alert_type
      | `Tls_failure of Tls.Engine.failure
      | `Msg of string ]

    let pp_error _ppf _err = assert false

    let recv flow buf =
      IO.catch (fun () ->
          read flow buf >>= function
          | 0 -> return (Ok `End_of_flow)
          | n -> return (Ok (`Input n)))
      @@ function
      | Tls_alert alert -> return (Error (`Tls_alert alert))
      | Tls_failure failure -> return (Error (`Tls_failure failure))
      | M0.Local_failure err -> return (Error (`Msg err))
      | exn -> fail exn

    let send flow buf =
      IO.catch (fun () ->
          write flow buf >>= fun () -> return (Ok (Cstruct.len buf)))
      @@ function
      | Tls_alert alert -> return (Error (`Tls_alert alert))
      | Tls_failure failure -> return (Error (`Tls_failure failure))
      | M0.Local_failure err -> return (Error (`Msg err))
      | exn -> fail exn

    let close flow =
      IO.catch (fun () -> close flow >>= fun () -> return (Ok ())) @@ function
      | Tls_alert alert -> return (Error (`Tls_alert alert))
      | Tls_failure failure -> return (Error (`Tls_failure failure))
      | M0.Local_failure err -> return (Error (`Msg err))
      | exn -> fail exn

    let connect (edn, conf) =
      Protocol.connect edn >>= function
      | Ok file_descr ->
          client_of_fd conf ?host:(Protocol.host_of_endpoint edn) file_descr
          >>= fun flow -> return (Ok flow)
      | Error err ->
          let msg = Format.asprintf "%a" Protocol.pp_error err in
          return (Error (`Msg msg))
  end

  module Make2
      (Protocol : PROTOCOL)
      (Service : Conduit.SERVICE
                   with type +'a io = 'a IO.t
                    and type flow = Protocol.flow) =
  struct
    module M0 = Make_S0_from_PROTOCOL (Protocol)

    type +'a io = 'a IO.t

    let ( >>= ) = IO.bind

    let return = IO.return

    type t = Service.t * Tls.Config.server

    type error = Service.error

    type configuration = Service.configuration * Tls.Config.server

    type flow = M0.file_descr protocol_with_tls

    let pp_error = Service.pp_error

    let init (cfg, tls) =
      Service.init cfg >>= function
      | Ok t -> return (Ok (t, tls))
      | Error _ as err -> return err

    include Make_drain_handshake (M0)

    let server_of_fd config fd =
      drain_handshake
        {
          state = `Active (Tls.Engine.server config);
          fd;
          linger = None;
          recv_buf = Cstruct.create 0x1000;
        }

    let accept (t, tls) =
      Service.accept t >>= function
      | Ok fd -> server_of_fd tls fd >>= fun flow -> return (Ok flow)
      | Error _ as err -> return err

    let stop (t, _) = Service.stop t
  end

  let underlying ({ fd; _ } : 'flow protocol_with_tls) = fd

  let handshake ({ state; _ } : 'flow protocol_with_tls) =
    match state with
    | `Active state -> Tls.Engine.handshake_in_progress state
    | _ -> false

  let protocol_with_tls :
      type edn flow.
      ?host_of_endpoint:(edn -> string option) ->
      (edn, flow) Conduit.protocol ->
      (edn * Tls.Config.client) Conduit.value * (edn * Tls.Config.client, flow protocol_with_tls) Conduit.protocol =
   fun ?(host_of_endpoint = fun _ -> None) protocol ->
    let module Protocol0 = (val Conduit.impl protocol) in
    let module Protocol1 = struct
      include Protocol0

      let host_of_endpoint = host_of_endpoint
    end in
    let module Protocol2 = Make1 (Protocol1) in
    Conduit.register ~name:"+tls" (module Protocol2)

  let credentials : Tls.Config.client Conduit.value = Conduit.info ~name:"tls-credentials"

  type 't service_with_tls = 't * Tls.Config.server

  let service_with_tls :
      type cfg t flow edn.
      (cfg, t, flow) Conduit.Service.t ->
      (edn, flow) Conduit.protocol ->
      (edn * Tls.Config.client, flow protocol_with_tls) Conduit.protocol ->
      ( cfg * Tls.Config.server,
        t service_with_tls,
        flow protocol_with_tls )
      Conduit.Service.t =
   fun service p0 p1 ->
    let module Service0 = (val Conduit.Service.impl service) in
    let module Protocol0 = (val Conduit.impl p0) in
    let module Service1 = Make2 (Protocol0) (Service0) in
    Conduit.Service.register (module Service1) p1
end
