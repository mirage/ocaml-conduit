(** Conduit with Async. *)

open Async_unix

module IO : Conduit.IO with type +'a t = 'a Async.Deferred.t

include
  Conduit.S
    with type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a io = 'a Async.Deferred.t

type ('a, 'b, 'c) service = ('a, 'b, 'c) Service.t
(** The type for async services. *)

val serve :
  ?timeout:int ->
  ?stop:unit Async.Deferred.t ->
  handler:(flow -> unit Async.Deferred.t) ->
  ('cfg, 't, 'v) service ->
  'cfg ->
  unit Async.Deferred.t
(** [serve ~handler t cfg] launches a service loop from the given configuration
    ['cfg]. By default, the service loop runs indefinitely.

    - If passed, [~stop] terminates the loop as soon as possible after it is
      determined.

    {[
      let stop, signal_stop = Ivar.(let v = create () in (read v, fill v)) i
      let loop = serve ~stop ~handler TCP.service cfg in
      Async_unix.Signal.handle [ Core.Signal.int ] ~f:(fun _ -> signal_stop ());
      loop
    ]}
    - If passed, [~timeout] specifies a maximum time to wait between accepting
      connections. *)

val serve_when_ready :
  ?timeout:int ->
  ?stop:unit Async.Deferred.t ->
  handler:(flow -> unit Async.Deferred.t) ->
  ('cfg, 't, 'v) service ->
  'cfg ->
  [ `Initialized of unit Async.Deferred.t ] Async.Deferred.t
(** An extension of {!serve} that promises a service loop computation that is
    ready to receive connections. The inner promise is then determined once the
    service loop has ended – by default, only when an error occurs.

    This is useful when subsequent actions are reliant on the service loop
    having begun, such as when testing with a client-server pair:

    {[
      let* (`Initialized server) = serve ~stop ~handler TCP.service cfg in
      Deferred.both server (client >>| signal_stop)
    ]} *)

val reader_and_writer_of_flow :
  flow -> (Async.Reader.t * Async.Writer.t) Async.Deferred.t

(** {2 Host's TCP/IP stack protocol with Async.} *)

module TCP : sig
  type endpoint =
    | Inet of Socket.Address.Inet.t
    | Unix of Socket.Address.Unix.t

  module Protocol : sig
    include PROTOCOL with type endpoint = endpoint

    val address : flow -> Socket.Address.t

    val reader : flow -> Reader.t

    val writer : flow -> Writer.t
  end

  val protocol : (Protocol.endpoint, Protocol.flow) protocol

  type configuration =
    | Listen :
        int option * ('a, 'b) Async.Tcp.Where_to_listen.t
        -> configuration

  module Service : SERVICE with type configuration = configuration

  val service : (configuration, Service.t, Protocol.flow) service

  val configuration :
    ?backlog:int -> ('a, 'listening_on) Tcp.Where_to_listen.t -> configuration

  val resolve : port:int -> endpoint resolver
end
