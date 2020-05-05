open Conduit_mirage

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
