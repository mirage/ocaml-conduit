open Conduit_async

type 'flow protocol_with_tls

val underlying : 'flow protocol_with_tls -> 'flow

val handshake : 'flow protocol_with_tls -> bool

val protocol_with_tls :
  key:'edn key ->
  'flow Witness.protocol ->
  ('edn * Tls.Config.client) key * 'flow protocol_with_tls Witness.protocol

type 'service service_with_tls

val service_with_tls :
  key:'edn key ->
  ('t * 'flow) Witness.service ->
  'flow protocol_with_tls Witness.protocol ->
  ('edn * Tls.Config.server) key
  * ('t service_with_tls * 'flow protocol_with_tls) Witness.service

module TCP : sig
  open Conduit_async_tcp

  val endpoint : (endpoint * Tls.Config.client) key

  val protocol : Protocol.flow protocol_with_tls Witness.protocol

  val configuration : (configuration * Tls.Config.server) key

  val service :
    (Service.t service_with_tls * Protocol.flow protocol_with_tls)
    Witness.service

  val resolv_conf :
    port:int ->
    config:Tls.Config.client ->
    (endpoint * Tls.Config.client) resolver
end
