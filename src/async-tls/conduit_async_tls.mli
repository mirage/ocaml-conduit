(** TLS layer with Conduit and Async. *)

open Conduit_async

type 'flow with_tls

val underlying : 'flow with_tls -> 'flow

val handshake : 'flow with_tls -> bool

val protocol_with_tls :
  ('edn, 'flow) protocol -> ('edn * Tls.Config.client, 'flow with_tls) protocol

type 'service service_with_tls

val service_with_tls :
  (_, 'flow with_tls) protocol ->
  ('cfg, 't, 'flow) Service.t ->
  ('cfg * Tls.Config.server, 't service_with_tls, 'flow with_tls) Service.t

(** {2 Composition between Host's TCP/IP stack protocol and TLS.} *)

module TCP : sig
  open Conduit_async.TCP

  val protocol : (endpoint * Tls.Config.client, Protocol.flow with_tls) protocol

  val service :
    ( configuration * Tls.Config.server,
      Service.t service_with_tls,
      Protocol.flow with_tls )
    service

  val resolve :
    port:int ->
    config:Tls.Config.client ->
    (endpoint * Tls.Config.client) resolver
end
