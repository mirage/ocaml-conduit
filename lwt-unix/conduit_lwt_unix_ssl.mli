(** Implementation of the SSL support (according [Lwt_ssl]) with
    [conduit-lwt-unix].

    This implementation assumes that underlying protocol used to compose with
    SSL must deliver a [Lwt_unix.file_descr] - such as [Conduit_lwt_unix_tcp].
    From that, we are able to compose your protocol with [Lwt_ssl] such as:

    {[
      let ssl_endpoint, ssl_protocol =
        protocol_with_ssl ~key:TCP.endpoint TCP.protocol

      let ssl_configuration, ssl_service =
        service_with_ssl ~key:TCP.configuration TCP.service
          ~file_descr:TCP.file_descr ssl_protocol
    ]}

    Then, TCP + SSL is available as any others [conduit] protocols or services
    registered.

    {b NOTE}: [close] implementation properly closes an SSL connection with
    [Ssl.ssl_shutdown] AND properly closes the underlying file-descriptor by
    itself. From a given implementation of a protocol like [TCP], [TCP.close]
    will never be called by the SSL layer - but the file-descriptor will be
    closed.

    {b NOTE}: [verify] is called after a call to [flow] (which should do the
    [connect] call). So, nothing was exchanged between you and your peer at this
    time - even the handshake. *)

open Conduit_lwt_unix

type ('edn, 'flow) endpoint = {
  context : Ssl.context;
  endpoint : 'edn;
  verify :
    Ssl.context -> 'flow -> (Lwt_ssl.socket, [ `Verify of string ]) result Lwt.t;
}

val endpoint :
  file_descr:('flow -> Lwt_unix.file_descr) ->
  context:Ssl.context ->
  ?verify:
    (Ssl.context ->
    'flow ->
    (Lwt_ssl.socket, [ `Verify of string ]) result Lwt.t) ->
  'edn ->
  ('edn, 'flow) endpoint
(** [endpoint ~file_descr ~context ?verify edn] returns an {i endpoint} needed
    to initialize a SSL connection from a [Lwt.file_descr]. Even if [endpoint]
    is abstracted over the type of the ['flow], we must be able to extract an
    [Lwt_unix.file_descr] from it.

    [verify] is the function called just after the initialization of the
    underlying ['flow]. It permits to request a verification such as the {i
    hostname} with your peer. *)

val protocol_with_ssl :
  ('edn, 'flow) Client.protocol ->
  (('edn, 'flow) endpoint, Lwt_ssl.socket) Client.protocol
(** [protocol_with_ssl ~key protocol] returns a representation of the given
    protocol with SSL. *)

type 't master
(** Type of the {i master} socket. *)

val service_with_ssl :
  ('cfg, 't * 'flow) Service.service ->
  file_descr:('flow -> Lwt_unix.file_descr) ->
  ('edn, Lwt_ssl.socket) Client.protocol ->
  (Ssl.context * 'cfg, 't master * Lwt_ssl.socket) Service.service
(** [service_with_ssl ~key service ~file_descr ssl_protocol] returns a
    representation of the given service with SSL. The service deliver an SSL
    flow which must be described by a [Lwt_ssl.socket Witness.protocol] (eg.
    {!protocol_with_ssl}).

    [file_descr] is used to extract from the given ['flow] delivered by our
    service a [Lwt_unix.file_descr] needed to create a [Lwt_ssl.socket]. *)

module TCP : sig
  open Conduit_lwt_unix_tcp

  val protocol :
    ( (Lwt_unix.sockaddr, Protocol.flow) endpoint,
      Lwt_ssl.socket )
    Client.protocol

  val service :
    ( Ssl.context * configuration,
      Server.t master * Lwt_ssl.socket )
    Service.service

  type verify =
    Ssl.context ->
    Protocol.flow ->
    (Lwt_ssl.socket, [ `Verify of string ]) result Lwt.t

  val resolv_conf :
    port:int ->
    context:Ssl.context ->
    ?verify:verify ->
    (Lwt_unix.sockaddr, Protocol.flow) endpoint Client.resolver
end
