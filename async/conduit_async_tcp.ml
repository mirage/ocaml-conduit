open Async
open Async_unix

type endpoint = Inet of Socket.Address.Inet.t | Unix of Socket.Address.Unix.t

module Protocol = struct
  type input = Cstruct.t

  type output = Cstruct.t

  type +'a s = 'a Async.Deferred.t

  type flow =
    | Socket : {
        address : [< Socket.Address.t ];
        socket : ([ `Active ], [< Socket.Address.t ]) Socket.t;
        reader : Async.Reader.t;
        writer : Async.Writer.t;
      }
        -> flow

  let address (Socket { address; _ }) =
    match address with #Socket.Address.t as addr -> addr

  let reader (Socket { reader; _ }) = reader

  let writer (Socket { writer; _ }) = writer

  type nonrec endpoint = endpoint =
    | Inet of Socket.Address.Inet.t
    | Unix of Socket.Address.Unix.t

  type error = Core.Error.t

  let pp_error = Core.Error.pp

  let connect edn =
    let connect = function
      | Inet address ->
          Tcp.connect (Tcp.Where_to_connect.of_inet_address address)
          >>| fun (socket, reader, writer) ->
          Socket { address; socket; reader; writer }
      | Unix address ->
          Tcp.connect (Tcp.Where_to_connect.of_unix_address address)
          >>| fun (socket, reader, writer) ->
          Socket { address; socket; reader; writer } in
    Monitor.try_with (fun () -> connect edn) >>= function
    | Ok _ as v -> Async.return v
    | Error exn -> Async.return (Error (Core.Error.of_exn exn))

  let of_cstruct raw =
    let { Cstruct.buffer; off; len } = raw in
    Core.Bigsubstring.create ~pos:off ~len buffer

  (* XXX(dinosaure): as [lwt] and seems required for [conduit-tls], [recv] wants to read
     as much as possible. Due to underlying non-blocking socket, even if we reached [`Eof],
     we must retry to read until we have something or the underlying socket was closed. *)
  let rec recv (Socket { socket; reader; _ } as flow) raw =
    Monitor.try_with (fun () ->
        Reader.read_bigsubstring reader (of_cstruct raw))
    >>= function
    | Error err ->
        Reader.close reader >>= fun () ->
        Async.return (Error (Core.Error.of_exn err))
    | Ok (`Ok n) -> Async.return (Ok (`Input n))
    | Ok `Eof -> (
        Fd.ready_to (Socket.fd socket) `Read >>= function
        | `Bad_fd | `Closed -> Async.return (Ok `End_of_input)
        | `Ready -> Scheduler.yield () >>= fun () -> recv flow raw)

  let send (Socket { writer; _ }) raw =
    Writer.write_bigsubstring writer (of_cstruct raw) ;
    Writer.flushed writer >>= fun () -> Async.return (Ok (Cstruct.len raw))

  let close (Socket { socket; reader; writer; _ }) =
    (* XXX(dinosaure): we should be protected against the double-close. *)
    if Reader.is_closed reader
       && Writer.is_closed writer
       && Fd.is_closed (Socket.fd socket)
    then Async.return (Ok ())
    else (
      Socket.shutdown socket `Both ;
      Reader.close reader >>= fun () ->
      Writer.close writer >>= fun () -> Async.return (Ok ()))
end

let protocol = Conduit_async.Client.register ~protocol:(module Protocol)

type configuration = Listen : ('a, 'b) Tcp.Where_to_listen.t -> configuration

module Server = struct
  type +'a s = 'a Async.Deferred.t

  type flow = Protocol.flow

  type error = Exn of [ `Make | `Accept ] * exn | Socket_closed

  let pp_error ppf = function
    | Exn (`Make, exn) ->
        Format.fprintf ppf "Got an exception while making socket: %s"
          (Printexc.to_string exn)
    | Exn (`Accept, exn) ->
        Format.fprintf ppf "Got an exception while accepting socket: %s"
          (Printexc.to_string exn)
    | Socket_closed -> Format.fprintf ppf "Socket closed"

  type nonrec configuration = configuration

  type t =
    | Master : ([ `Passive ], ([< Socket.Address.t ] as 'a)) Socket.t * 'a -> t

  let close_socket_on_error ~process socket ~f =
    Monitor.try_with f >>| function
    | Ok v -> Ok v
    | Error exn ->
        Async.don't_wait_for (Unix.close (Socket.fd socket)) ;
        Error (Exn (process, exn))

  type socket_type =
    | Socket_type :
        ([< Socket.Address.t ] as 'a) Socket.Type.t * 'a
        -> socket_type

  let ( >>? ) x f =
    x >>= function Ok x -> f x | Error _ as err -> Async.return err

  let make (Listen where_to_listen) =
    let (Socket_type (socket_type, addr)) =
      match Tcp.Where_to_listen.address where_to_listen with
      | `Inet _ as addr -> Socket_type (Socket.Type.tcp, addr)
      | `Unix _ as addr -> Socket_type (Socket.Type.unix, addr) in
    let socket = Socket.create socket_type in
    let f () = Socket.bind socket addr >>| Socket.listen in
    close_socket_on_error ~process:`Make socket ~f >>? fun socket ->
    Async.return (Ok (Master (socket, addr)))

  let accept (Master (socket, _)) =
    Socket.accept socket >>= function
    | `Ok (socket, address) ->
        let reader = Reader.create (Socket.fd socket) in
        let writer = Writer.create (Socket.fd socket) in
        let flow = Protocol.Socket { socket; reader; writer; address } in
        Async.return (Ok flow)
    | `Socket_closed -> Async.return (Error Socket_closed)

  let close (Master (socket, _)) =
    Fd.close (Socket.fd socket) >>= fun () -> Async.return (Ok ())
end

let service = Conduit_async.Service.register ~service:(module Server)

let resolv_conf ~port domain_name =
  Monitor.try_with (fun () ->
      Unix.Inet_addr.of_string_or_getbyname (Domain_name.to_string domain_name))
  >>= function
  | Ok inet_addr ->
      let inet_addr = Socket.Address.Inet.create inet_addr ~port in
      Async.return (Some (Inet inet_addr))
  | _ -> Async.return None
