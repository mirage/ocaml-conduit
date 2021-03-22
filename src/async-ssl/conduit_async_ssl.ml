open Async_ssl
open Async
open Core

let ( >>? ) x f =
  x >>= function Ok x -> f x | Error _ as err -> Async.return err

let reword_error f = function Ok _ as v -> v | Error err -> Error (f err)

let teardown_connection reader writer =
  Writer.close ~force_close:Clock.(after (sec 30.)) writer >>= fun () ->
  Reader.close reader

let reader_writer_pipes reader writer =
  let reader_pipe_reader, reader_pipe_writer = Pipe.create () in
  let writer_pipe = Writer.pipe writer in
  Async.upon (Reader.transfer reader reader_pipe_writer) (fun () ->
      teardown_connection reader writer >>> fun () ->
      Pipe.close reader_pipe_writer) ;
  Async.upon (Pipe.closed writer_pipe) (fun () ->
      Deferred.choose
        [
          Deferred.choice Clock.(after (sec 30.)) (fun () -> ());
          Deferred.choice (Pipe.downstream_flushed writer_pipe)
            (fun (_ : Pipe.Flushed_result.t) -> ());
        ]
      >>> fun () -> don't_wait_for (teardown_connection reader writer)) ;
  (reader_pipe_reader, writer_pipe)

let reader_writer_of_pipes app_rd app_wr =
  Reader.of_pipe (Info.of_string "async-conduit-ssl-reader") app_rd
  >>= fun app_reader ->
  Async.upon (Reader.close_finished app_reader) (fun () ->
      Pipe.close_read app_rd) ;
  Writer.of_pipe (Info.of_string "async-conduit-ssl-writer") app_wr
  >>= fun (app_writer, _) ->
  Writer.set_raise_when_consumer_leaves app_writer false ;
  Async.return (app_reader, app_writer)

type context = {
  version : Ssl.Version.t option;
  options : Ssl.Opt.t list option;
  name : string option;
  hostname : string option;
  allowed_ciphers :
    [ `Only of string list | `Openssl_default | `Secure ] option;
  ca_file : string option;
  ca_path : string option;
  crt_file : string option;
  key_file : string option;
  session : Ssl.Session.t option;
  verify_modes : Verify_mode.t list option;
  verify : (Ssl.Connection.t -> bool Async.Deferred.t) option;
}

let context ?version ?options ?name ?hostname ?allowed_ciphers ?ca_file ?ca_path
    ?crt_file ?key_file ?session ?verify_modes ?verify () =
  {
    version;
    options;
    name;
    hostname;
    allowed_ciphers;
    ca_file;
    ca_path;
    crt_file;
    key_file;
    session;
    verify_modes;
    verify;
  }

type 'flow with_ssl = {
  connection : Ssl.Connection.t;
  reader : Reader.t;
  writer : Writer.t;
  underlying : 'flow;
}

module Protocol (Protocol : sig
  include Conduit_async.PROTOCOL

  val reader : flow -> Reader.t

  val writer : flow -> Writer.t
end) =
struct
  type input = Cstruct.t

  type output = Cstruct.t

  type +'a io = 'a Async.Deferred.t

  type endpoint = context * Protocol.endpoint

  type flow = Protocol.flow with_ssl

  exception Invalid_connection

  type error = Core of Core.Error.t | Protocol of Protocol.error

  let pp_error ppf = function
    | Core err -> Core.Error.pp ppf err
    | Protocol err -> Protocol.pp_error ppf err

  let connect
      ( {
          version;
          options;
          name;
          hostname;
          allowed_ciphers;
          ca_file;
          ca_path;
          crt_file;
          key_file;
          session;
          verify_modes;
          verify;
        },
        edn ) =
    Protocol.connect edn >>| reword_error (fun err -> Protocol err)
    >>? fun underlying ->
    let reader = Protocol.reader underlying in
    let writer = Protocol.writer underlying in

    let net_to_ssl, ssl_to_net = reader_writer_pipes reader writer in
    let app_to_ssl, app_writer = Pipe.create () in
    let app_reader, ssl_to_app = Pipe.create () in
    let verify_connection =
      match verify with None -> Fn.const (return true) | Some verify -> verify
    in
    Monitor.try_with_join_or_error (fun () ->
        Ssl.client ?version ?options ?name ?hostname ?allowed_ciphers ?ca_file
          ?ca_path ?crt_file ?key_file ?session ?verify_modes ~app_to_ssl
          ~ssl_to_app ~net_to_ssl ~ssl_to_net ())
    >>| reword_error (fun err -> Core err)
    >>= function
    | Error _ as err ->
        teardown_connection reader writer >>= fun () -> Async.return err
    | Ok conn -> (
        verify_connection conn >>= function
        | true ->
            reader_writer_of_pipes app_reader app_writer
            >>= fun (app_reader, app_writer) ->
            Async.return
              (Ok
                 {
                   connection = conn;
                   reader = app_reader;
                   writer = app_writer;
                   underlying;
                 })
        | false ->
            teardown_connection reader writer >>= fun () ->
            Async.return (Error (Core (Core.Error.of_exn Invalid_connection))))

  let of_cstruct raw =
    let { Cstruct.buffer; off; len } = raw in
    Core.Bigsubstring.create ~pos:off ~len buffer

  let recv { reader; _ } raw =
    Reader.read_bigsubstring reader (of_cstruct raw) >>= function
    | `Eof -> Async.return (Ok `End_of_flow)
    | `Ok n -> Async.return (Ok (`Input n))

  let send { writer; _ } raw =
    Writer.write_bigsubstring writer (of_cstruct raw) ;
    Async.return (Ok (Cstruct.len raw))

  let close { reader; writer; _ } =
    Reader.close reader >>= fun () ->
    Writer.close writer >>= fun () -> Async.return (Ok ())
end

let protocol_with_ssl :
    type edn flow.
    reader:(flow -> Reader.t) ->
    writer:(flow -> Writer.t) ->
    (edn, flow) Conduit_async.protocol ->
    (context * edn, flow with_ssl) Conduit_async.protocol =
 fun ~reader ~writer protocol ->
  let module F = (val Conduit_async.impl protocol) in
  let module Flow = struct
    include F

    let reader = reader

    let writer = writer
  end in
  let module M = Protocol (Flow) in
  Conduit_async.register (module M)

module Make (Service : sig
  include Conduit_async.SERVICE

  val reader : flow -> Reader.t

  val writer : flow -> Writer.t
end) =
struct
  type +'a io = 'a Async.Deferred.t

  type error =
    | Service of Service.error
    | Core of Core.Error.t
    | Missing_crt_or_key

  let pp_error ppf = function
    | Service err -> Service.pp_error ppf err
    | Core err -> Core.Error.pp ppf err
    | Missing_crt_or_key ->
        Format.fprintf ppf "Missing crt of key values into context"

  type configuration = context * Service.configuration

  type t = context * Service.t

  type flow = Service.flow with_ssl

  let init (context, edn) =
    match (context.crt_file, context.key_file) with
    | None, None | Some _, None | None, Some _ ->
        Async.return (Error Missing_crt_or_key)
    | _ -> (
        Service.init edn >>= function
        | Ok t -> Async.return (Ok (context, t))
        | Error err -> Async.return (Error (Service err)))

  let accept
      ( {
          version;
          options;
          name;
          allowed_ciphers;
          ca_file;
          ca_path;
          crt_file;
          key_file;
          verify_modes;
          _;
        },
        service ) =
    Service.accept service >>= function
    | Error err -> Async.return (Error (Service err))
    | Ok flow -> (
        let crt_file, key_file =
          match (crt_file, key_file) with
          | Some crt_file, Some key_file -> (crt_file, key_file)
          | _ -> assert false in
        let reader = Service.reader flow in
        let writer = Service.writer flow in
        let net_to_ssl, ssl_to_net = reader_writer_pipes reader writer in
        let app_to_ssl, app_writer = Pipe.create () in
        let app_reader, ssl_to_app = Pipe.create () in
        Ssl.server ?version ?options ?name ?allowed_ciphers ?ca_file ?ca_path
          ~crt_file ~key_file ?verify_modes ~app_to_ssl ~ssl_to_app ~net_to_ssl
          ~ssl_to_net ()
        >>= function
        | Error error ->
            teardown_connection reader writer >>= fun () ->
            Async.return (Error (Core error))
        | Ok conn ->
            reader_writer_of_pipes app_reader app_writer
            >>| fun (app_reader, app_writer) ->
            Ok
              {
                underlying = flow;
                reader = app_reader;
                writer = app_writer;
                connection = conn;
              })

  let stop (_, t) =
    Service.stop t >>= function
    | Error err -> Async.return (Error (Service err))
    | Ok _ as v -> Async.return v
end

let service_with_ssl :
    type cfg edn t flow.
    (cfg, t, flow) Conduit_async.Service.t ->
    reader:(flow -> Reader.t) ->
    writer:(flow -> Writer.t) ->
    (edn, flow with_ssl) Conduit_async.protocol ->
    (context * cfg, context * t, flow with_ssl) Conduit_async.Service.t =
 fun service ~reader ~writer protocol ->
  let module S = (val Conduit_async.Service.impl service) in
  let module Service = struct
    include S

    let reader = reader

    let writer = writer
  end in
  let module M = Make (Service) in
  Conduit_async.Service.register (module M) protocol

module TCP = struct
  open Conduit_async.TCP

  let protocol =
    protocol_with_ssl ~reader:Protocol.reader ~writer:Protocol.writer protocol

  let service ?listening_on () =
    service_with_ssl (service ?listening_on ()) ~reader:Protocol.reader ~writer:Protocol.writer
      protocol

  let configuration ~context ?backlog listen =
    (context, configuration ?backlog listen)

  let resolve ~port ~context domain_name =
    resolve ~port domain_name >>| function
    | Some edn -> Some (context, edn)
    | None -> None
end
