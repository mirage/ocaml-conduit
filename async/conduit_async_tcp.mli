open Async
open Conduit_async

type endpoint = Inet of Socket.Address.Inet.t | Unix of Socket.Address.Unix.t

module Protocol : sig
  include Conduit_async.Client.PROTOCOL with type endpoint = endpoint

  val address : flow -> Socket.Address.t

  val reader : flow -> Reader.t

  val writer : flow -> Writer.t
end

val protocol : (Protocol.endpoint, Protocol.flow) Client.protocol

type configuration = Listen : ('a, 'b) Tcp.Where_to_listen.t -> configuration

module Server : Service.SERVICE with type configuration = configuration

val service : (configuration, Server.t * Protocol.flow) Service.service

val resolv_conf : port:int -> endpoint Client.resolver
