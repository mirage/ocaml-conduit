open Core
open Async
open Async_ssl

let verify_certificate connection =
  match Ssl.Connection.peer_certificate connection with
  | None -> return false
  | Some (Error _) -> return false
  | Some (Ok _) -> return true

module V1 = struct
  module Ssl = struct
    module Config = struct
      type t = {
        version : Ssl.Version.t option;
        name : string option;
        ca_file : string option;
        ca_path : string option;
        session : Ssl.Session.t option sexp_opaque;
        verify : (Ssl.Connection.t -> bool Deferred.t) option;
      } [@@deriving sexp]

      let verify_certificate = verify_certificate

      let create ?version ?name ?ca_file ?ca_path ?session ?verify () =
        { version; name; ca_file; ca_path; session; verify}
    end

    let connect cfg r w =
      let {Config.version; name; ca_file; ca_path; session; verify} = cfg in
      let net_to_ssl = Reader.pipe r in
      let ssl_to_net = Writer.pipe w in
      let app_to_ssl, app_wr = Pipe.create () in
      let app_rd, ssl_to_app = Pipe.create () in
      let verify_connection = match verify with
        | None -> Fn.const (return true)
        | Some f -> f
      in
      Ssl.client
        ?version
        ?name
        ?ca_file
        ?ca_path
        ?session
        ~app_to_ssl
        ~ssl_to_app
        ~net_to_ssl
        ~ssl_to_net
        ()
      |> Deferred.Or_error.ok_exn
      >>= fun conn ->
      verify_connection conn >>= function
      | false ->
        Ssl.Connection.close conn ;
        Pipe.close_read app_rd ;
        Writer.close w >>= fun () ->
        failwith "Connection verification failed."
      | true ->
        Reader.of_pipe (Info.of_string "async_conduit_ssl_reader") app_rd >>= fun app_reader ->
        Writer.of_pipe (Info.of_string "async_conduit_ssl_writer") app_wr >>| fun (app_writer,_) ->
        don't_wait_for begin
          Deferred.all_unit [
            Writer.close_finished app_writer ;
            Reader.close_finished app_reader ;
          ] >>= fun () ->
          Ssl.Connection.close conn ;
          Pipe.close_read app_rd ;
          Writer.close w ;
        end ;
        (app_reader, app_writer)

    let listen ?(version=Ssl.Version.Tlsv1_2) ?ca_file ?ca_path ~crt_file ~key_file r w =
      let net_to_ssl = Reader.pipe r in
      let ssl_to_net = Writer.pipe w in
      let app_to_ssl, app_wr = Pipe.create () in
      let app_rd, ssl_to_app = Pipe.create () in
      Ssl.server
        ?ca_file
        ?ca_path
        ~version
        ~crt_file
        ~key_file
        ~app_to_ssl
        ~ssl_to_app
        ~net_to_ssl
        ~ssl_to_net
        ()
      |> Deferred.Or_error.ok_exn
      >>= fun conn ->
      Reader.of_pipe (Info.of_string "async_conduit_ssl_reader") app_rd >>= fun app_reader ->
      Writer.of_pipe (Info.of_string "async_conduit_ssl_writer") app_wr >>| fun (app_writer,_) ->
      don't_wait_for begin
        Deferred.all_unit [
          Reader.close_finished app_reader;
          Writer.close_finished app_writer
        ] >>= fun () ->
        Ssl.Connection.close conn ;
        Pipe.close_read app_rd ;
        Writer.close w ;
      end;
      (app_reader, app_writer)

    type session = Ssl.Session.t sexp_opaque [@@deriving sexp]
    type version = Ssl.Version.t  [@@deriving sexp]
    type connection = Ssl.Connection.t sexp_opaque [@@deriving sexp]
  end
end

module V2 = struct
  module Ssl = struct
    type allowed_ciphers =
      [ `Only of string list | `Openssl_default | `Secure ]
      [@@deriving sexp]

    module Config = struct
      type t = {
        version : Ssl.Version.t option;
        options: Ssl.Opt.t list option;
        name : string option;
        hostname : string option;
        allowed_ciphers: allowed_ciphers option;
        ca_file : string option;
        ca_path : string option;
        crt_file : string option;
        key_file : string option;
        session : Ssl.Session.t option sexp_opaque;
        verify_modes:Verify_mode.t sexp_opaque list option;
        verify : (Ssl.Connection.t -> bool Deferred.t) option;
      } [@@deriving sexp_of]

      let verify_certificate = verify_certificate

      let create
          ?version ?options ?name ?hostname ?allowed_ciphers
          ?ca_file ?ca_path ?crt_file ?key_file
          ?session ?verify_modes ?verify () =
        { version; options; name; hostname; allowed_ciphers;
          ca_file; ca_path; crt_file; key_file; session; verify_modes;
          verify}
    end

    let connect ?(cfg=Config.create ()) r w =
      let { Config.version; options; name; hostname;
            allowed_ciphers; ca_file; ca_path;
            crt_file; key_file; session; verify_modes; verify } = cfg in
      let net_to_ssl = Reader.pipe r in
      let ssl_to_net = Writer.pipe w in
      let app_to_ssl, app_wr = Pipe.create () in
      let app_rd, ssl_to_app = Pipe.create () in
      let verify_connection = match verify with
        | None -> Fn.const (return true)
        | Some f -> f
      in
      Ssl.client
        ?version
        ?options
        ?name
        ?hostname
        ?allowed_ciphers
        ?ca_file
        ?ca_path
        ?crt_file
        ?key_file
        ?session
        ?verify_modes
        ~app_to_ssl
        ~ssl_to_app
        ~net_to_ssl
        ~ssl_to_net
        ()
      |> Deferred.Or_error.ok_exn
      >>= fun conn ->
      verify_connection conn >>= function
      | false ->
        Ssl.Connection.close conn ;
        Pipe.close_read app_rd ;
        Writer.close w >>= fun () ->
        failwith "Connection verification failed."
      | true ->
        Reader.of_pipe (Info.of_string "async_conduit_ssl_reader") app_rd >>= fun app_reader ->
        Writer.of_pipe (Info.of_string "async_conduit_ssl_writer") app_wr >>| fun (app_writer,_) ->
        don't_wait_for begin
          Deferred.all_unit [
            Writer.close_finished app_writer ;
            Reader.close_finished app_reader ;
          ] >>= fun () ->
          Ssl.Connection.close conn ;
          Pipe.close_read app_rd ;
          Writer.close w ;
        end ;
        (app_reader, app_writer)

    let listen
        { Config.version; options; name; allowed_ciphers; ca_file; ca_path;
          crt_file; key_file; verify_modes ; _ } r w =
      let crt_file, key_file =
        match crt_file, key_file with
        | Some crt_file, Some key_file -> crt_file, key_file
        | _ -> invalid_arg "Conduit_async_ssl.ssl_listen: crt_file and \
                            key_file must be specified in cfg." in
      let net_to_ssl = Reader.pipe r in
      let ssl_to_net = Writer.pipe w in
      let app_to_ssl, app_wr = Pipe.create () in
      let app_rd, ssl_to_app = Pipe.create () in
      Ssl.server
        ?version
        ?options
        ?name
        ?allowed_ciphers
        ?ca_file
        ?ca_path
        ~crt_file
        ~key_file
        ?verify_modes
        ~app_to_ssl
        ~ssl_to_app
        ~net_to_ssl
        ~ssl_to_net
        ()
      |> Deferred.Or_error.ok_exn
      >>= fun conn ->
      Reader.of_pipe (Info.of_string "async_conduit_ssl_reader") app_rd >>= fun app_reader ->
      Writer.of_pipe (Info.of_string "async_conduit_ssl_writer") app_wr >>| fun (app_writer,_) ->
      don't_wait_for begin
        Deferred.all_unit [
          Reader.close_finished app_reader;
          Writer.close_finished app_writer
        ] >>= fun () ->
        Ssl.Connection.close conn ;
        Pipe.close_read app_rd ;
        Writer.close w ;
      end;
      (app_reader, app_writer)

    type verify_mode = Ssl.Verify_mode.t [@@deriving sexp_of]
    type session = Ssl.Session.t sexp_opaque [@@deriving sexp_of]
    type version = Ssl.Version.t [@@deriving sexp]
    type connection = Ssl.Connection.t [@@deriving sexp_of]
    type opt = Ssl.Opt.t [@@deriving sexp]
  end
end
