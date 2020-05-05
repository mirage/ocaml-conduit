(** Implementation of the TLS support (according [ocaml-tls]) with
    [conduit-lwt-unix].

    This implementation is a {i specialization} of [conduit-tls] with
    [conduit-lwt-unix]. Underlying protocol or service can be anything into the
    scope of [conduit-lwt]/[conduit-lwt-unix].

    For more details about behaviours, you should look into [conduit-tls]. *)

open Conduit_lwt_unix

type 'flow protocol_with_tls

val underlying : 'flow protocol_with_tls -> 'flow
(** [underlying tls_flow] returns the underlying [flow] used with TLS. *)

val handshake : 'flow protocol_with_tls -> bool
(** [handshake flow] returns [true] if the handshake is processing. Otherwise,
    it returns [false]. *)

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
  open Conduit_lwt_unix_tcp

  val endpoint : (Lwt_unix.sockaddr * Tls.Config.client) key

  val protocol : Protocol.flow protocol_with_tls Witness.protocol

  val configuration : (configuration * Tls.Config.server) key

  val service :
    (Service.t service_with_tls * Protocol.flow protocol_with_tls)
    Witness.service

  val resolv_conf :
    port:int ->
    config:Tls.Config.client ->
    (Lwt_unix.sockaddr * Tls.Config.client) resolver
end
