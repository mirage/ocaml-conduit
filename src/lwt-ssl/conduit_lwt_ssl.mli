(** Implementation of the SSL support (according [Lwt_ssl]) with [conduit-lwt].

    This implementation assumes that underlying protocol used to compose with
    SSL must deliver a [Lwt_unix.file_descr] - such as [Conduit_lwt.TCP]. From
    that, we are able to compose your protocol with [Lwt_ssl] such as:

    {[
      let ssl_protocol = protocol_with_ssl TCP.protocol

      let ssl_service =
        service_with_ssl TCP.service ~file_descr:TCP.file_descr ssl_protocol
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
    time - even the handshake. It permits to fill the SSL socket with some
    information such as the hostname of the peer with
    [Ssl.set_client_SNI_hostname]. *)

open Conduit_lwt

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
    underlying ['flow]. It permits to request a verification such as the
    {i hostname} with your peer. *)

val protocol_with_ssl :
  ('edn, 'flow) protocol -> (('edn, 'flow) endpoint, Lwt_ssl.socket) protocol
(** [protocol_with_ssl ~key protocol] returns a representation of the given
    protocol with SSL. *)

type 't service
(** The type for SSL services. *)

val service_with_ssl :
  ('cfg, 't, 'flow) Service.t ->
  file_descr:('flow -> Lwt_unix.file_descr) ->
  ('edn, Lwt_ssl.socket) protocol ->
  (Ssl.context * 'cfg, 't service, Lwt_ssl.socket) Service.t
(** [service_with_ssl ~key service ~file_descr ssl_protocol] returns a
    representation of the given service with SSL. The service deliver an SSL
    flow which must be described by a [Lwt_ssl.socket Witness.protocol] (eg.
    {!protocol_with_ssl}).

    [file_descr] is used to extract from the given ['flow] delivered by our
    service a [Lwt_unix.file_descr] needed to create a [Lwt_ssl.socket]. *)

module TCP : sig
  open Conduit_lwt.TCP

  val protocol :
    ((Lwt_unix.sockaddr, Protocol.flow) endpoint, Lwt_ssl.socket) protocol

  val service :
    ( Ssl.context * configuration,
      Service.t service,
      Lwt_ssl.socket )
    Conduit_lwt.service

  type verify =
    Ssl.context ->
    Protocol.flow ->
    (Lwt_ssl.socket, [ `Verify of string ]) result Lwt.t

  val resolve :
    port:int ->
    context:Ssl.context ->
    ?verify:verify ->
    (Lwt_unix.sockaddr, Protocol.flow) endpoint resolver

  type Conduit_lwt.flow += T of Lwt_ssl.socket
end
