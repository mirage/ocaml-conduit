open Async
open Conduit_async

type endpoint = Inet of Socket.Address.Inet.t | Unix of Socket.Address.Unix.t

module Protocol : sig
  include Conduit_async.PROTOCOL

  val address : flow -> Socket.Address.t

  val reader : flow -> Reader.t

  val writer : flow -> Writer.t
end

val endpoint : endpoint key

val protocol : Protocol.flow Witness.protocol

type configuration = Listen : ('a, 'b) Tcp.Where_to_listen.t -> configuration

module Service : SERVICE with type endpoint = configuration

val configuration : configuration key

val service : (Service.t * Protocol.flow) Witness.service

val resolv_conf : port:int -> endpoint resolver
