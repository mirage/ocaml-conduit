(** Conduit with Async. *)

open Async_unix

module IO : Conduit.IO with type +'a t = 'a Async.Deferred.t

include
  Conduit.S
    with type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a io = 'a Async.Deferred.t

val serve :
  handler:('flow -> unit Async.Deferred.t) ->
  service:('cfg, 'master, 'flow) Service.service ->
  'cfg ->
  unit Async.Condition.t * unit Async.Deferred.t

val reader_and_writer_of_flow :
  flow -> (Async.Reader.t * Async.Writer.t) Async.Deferred.t

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
    | Listen : ('a, 'b) Async.Tcp.Where_to_listen.t -> configuration

  module Server : SERVICE with type configuration = configuration

  val service : (configuration, Server.t, Protocol.flow) Service.service

  val resolve : port:int -> endpoint resolver
end
