(** TLS layer with Conduit and Async. *)

open Conduit_async

type 'flow protocol_with_tls

val underlying : 'flow protocol_with_tls -> 'flow

val handshake : 'flow protocol_with_tls -> bool

val protocol_with_tls :
  ?host_of_endpoint:('edn -> string option) ->
  ('edn, 'flow) protocol ->
  ('edn * Tls.Config.client) value
  * ('edn * Tls.Config.client, 'flow protocol_with_tls) protocol

type 'service service_with_tls

val service_with_tls :
  ('cfg, 't, 'flow) Service.t ->
  ('edn, 'flow) protocol ->
  ('edn * Tls.Config.client, 'flow protocol_with_tls) protocol ->
  ( 'cfg * Tls.Config.server,
    't service_with_tls,
    'flow protocol_with_tls )
  Service.t

(** {2 Composition between Host's TCP/IP stack protocol and TLS.} *)

module TCP : sig
  open Conduit_async.TCP

  val protocol :
    (endpoint * Tls.Config.client, Protocol.flow protocol_with_tls) protocol

  val service :
    ( configuration * Tls.Config.server,
      Service.t service_with_tls,
      Protocol.flow protocol_with_tls )
    service

  val configuration :
    config:Tls.Config.server ->
    ?backlog:int ->
    ('a, 'listening_on) Async_unix.Tcp.Where_to_listen.t ->
    configuration * Tls.Config.server

  val resolve : Conduit.context -> Conduit.context

  val credentials : Tls.Config.client -> Conduit.context -> Conduit.context

  val endpoint :
    endpoint -> Tls.Config.client -> Conduit.context -> Conduit.context
end
