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
  handler:(flow -> unit Async.Deferred.t) ->
  ('cfg, 't, 'v) service ->
  'cfg ->
  unit Async.Condition.t * (unit -> unit Async.Deferred.t)
(** [serve ~handler t cfg] creates an infinite service loop from the given
    configuration ['cfg]. It returns the {i promise} to launch the loop and a
    condition variable to stop the loop.

    {[
      let stop, loop = serve ~handler TCP.service cfg in
      Async_unix.Signal.handle [ Core.Signal.int ] ~f:(fun _sig ->
          Async.Condition.broadcast stop ()) ;
      loop ()
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

  val service :
    ?listening_on:[ `Inet of int | `Unix of string ] Async.Ivar.t ->
    unit ->
    (configuration, Service.t, Protocol.flow) service

  val configuration :
    ?backlog:int -> ('a, 'listening_on) Tcp.Where_to_listen.t -> configuration

  val resolve : port:int -> endpoint resolver
end
