(** Implementation of the TLS support (according [ocaml-tls]) with
    [conduit-lwt-unix].

    This implementation is a {i specialization} of [conduit-tls] with
    [conduit-lwt-unix]. Underlying protocol or service can be anything into the
    scope of [conduit-lwt]/[conduit-lwt-unix].

    For more details about behaviours, you should look into [conduit-tls]. *)

open Conduit_lwt

type 'flow protocol_with_tls

val underlying : 'flow protocol_with_tls -> 'flow
(** [underlying tls_flow] returns the underlying [flow] used with TLS. *)

val handshake : 'flow protocol_with_tls -> bool
(** [handshake flow] returns [true] if the handshake is processing. Otherwise,
    it returns [false]. *)

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
  open Conduit_lwt.TCP

  val protocol :
    ( Lwt_unix.sockaddr * Tls.Config.client,
      Protocol.flow protocol_with_tls )
    protocol

  type t =
    ( Lwt_unix.sockaddr * Tls.Config.client,
      Protocol.flow protocol_with_tls )
    Conduit.value

  type Conduit_lwt.flow += T of t

  val service :
    ( configuration * Tls.Config.server,
      Service.t service_with_tls,
      Protocol.flow protocol_with_tls )
    service

  val resolve :
    port:int ->
    config:Tls.Config.client ->
    (Lwt_unix.sockaddr * Tls.Config.client) resolver
end
