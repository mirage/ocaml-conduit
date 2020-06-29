open Conduit_async

type 'flow protocol_with_tls

val underlying : 'flow protocol_with_tls -> 'flow

val handshake : 'flow protocol_with_tls -> bool

val protocol_with_tls :
  ('edn, 'flow) protocol ->
  ('edn * Tls.Config.client, 'flow protocol_with_tls) protocol

type 'service service_with_tls

val service_with_tls :
  ('cfg, 't, 'flow) Service.service ->
  ('edn, 'flow protocol_with_tls) protocol ->
  ( 'cfg * Tls.Config.server,
    't service_with_tls,
    'flow protocol_with_tls )
  Service.service

module TCP : sig
  open Conduit_async.TCP

  val protocol :
    (endpoint * Tls.Config.client, Protocol.flow protocol_with_tls) protocol

  val service :
    ( configuration * Tls.Config.server,
      Server.t service_with_tls,
      Protocol.flow protocol_with_tls )
    Service.service

  val resolve :
    port:int ->
    config:Tls.Config.client ->
    (endpoint * Tls.Config.client) resolver
end
