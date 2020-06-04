open Conduit_mirage

type 'flow protocol_with_tls

val underlying : 'flow protocol_with_tls -> 'flow

val handshake : 'flow protocol_with_tls -> bool

val protocol_with_tls :
  ('edn, 'flow) Client.protocol ->
  ('edn * Tls.Config.client, 'flow protocol_with_tls) Client.protocol

type 'service service_with_tls

val service_with_tls :
  ('cfg, 't, 'flow) Service.service ->
  ('edn, 'flow protocol_with_tls) Client.protocol ->
  ( 'cfg * Tls.Config.server,
    't service_with_tls,
    'flow protocol_with_tls )
  Service.service
