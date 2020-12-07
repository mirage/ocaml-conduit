module IO : Conduit.IO with type +'a t = 'a Lwt.t

include
  Conduit.S
    with type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a io = 'a Lwt.t

val io_of_flow :
  flow -> Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel
(** [io_of_flow flow] creates an input flow and an output flow according to
    [Lwt_io]. This function, even if it creates something more usable is
    {b deprecated}. Indeed, [Lwt_io] has its own way to schedule [read] and
    [write]. It assumes (but it can not check) that [Conduit.recv flow] and
    [Conduit.send flow] are {i thread-safe}. They can be called concurrently. *)

type ('a, 'b, 'c) service = ('a, 'b, 'c) Service.t
(** The type for lwt services. *)

val serve :
  ?timeout:int ->
  ?stop:Lwt_switch.t ->
  handler:(flow -> unit Lwt.t) ->
  ('cfg, 'service, 'v) service ->
  'cfg ->
  unit Lwt.t
(** [serve ~handler service cfg] launches a [service] loop from the given
    configuration ['cfg]. By default, the service loop runs indefinitely.

    - If passed, [~stop] is a switch that terminates the service loop, for
      example to limit execution time to 10 seconds:

    {[
      let stop = Lwt_switch.create () in
      let loop = serve ~stop ~handler TCP.service cfg in
      Lwt.both (Lwt_unix.sleep 10. >>= fun () -> Lwt_switch.turn_off stop) loop
    ]}
    - If passed, [~timeout] specifies a maximum time to wait between accepting
      connections. *)

val serve_when_ready :
  ?timeout:int ->
  ?stop:Lwt_switch.t ->
  handler:(flow -> unit Lwt.t) ->
  ('cfg, 'service, 'v) service ->
  'cfg ->
  [ `Initialized of unit Lwt.t ] Lwt.t
(** An extension of {!serve} that promises a service loop computation that is
    ready to receive connections. The inner promise is then determined once the
    service loop has ended – by default, only when an error occurs.

    This is useful when subsequent actions are reliant on the service loop
    having begun, such as when testing with a client-server pair:

    {[
      let* (`Initialized server) = serve ~stop ~handler TCP.service cfg in
      Lwt.both server (client >|= signal_stop)
    ]} *)

module TCP : sig
  (** Implementation of TCP protocol as a client.

      Behaviours of [Protocol] differs from {i syscall} provided by [Lwt_unix].
      This is a description of what they currently do.

      {b NOTE}: [recv] does one and unique call of [Lwt_unix.read]. It returns
      what [Lwt_unix.read] returns with a special case when it returns [0]. In
      that case, we returns [`End_of_flow]. Any errors (exception) are handled
      and, in that case, we {i shutdown} the underlying socket.

      {b NOTE}: [close] calls [Lwt_unix.close] only one and unique time. Then,
      all subsequent calls of [recv] returns [`End_of_flow] and all subsequent
      calls of [send] returns an error.

      {b NOTE}: [send] tries to send in one call to [Lwt_unix.write] the given
      buffer. It returns how many bytes was transmitted, as [Lwt_unix.write]. It
      handles exception and {i shutdown} the connection when we got [ECONNRESET]
      or [EPIPE]. *)

  module Protocol : sig
    include
      PROTOCOL
        with type endpoint = Lwt_unix.sockaddr
         and type error =
              [ `Closed_by_peer
              | `Operation_not_permitted
              | `Address_already_in_use of Unix.sockaddr
              | `Cannot_assign_requested_address of Unix.sockaddr
              | `Address_family_not_supported_by_protocol of Unix.sockaddr
              | `Operation_already_in_progress
              | `Bad_address
              | `Network_is_unreachable
              | `Connection_timed_out
              | `Connection_refused
              | `Transport_endpoint_is_not_connected ]

    val file_descr : flow -> Lwt_unix.file_descr
    (** [file_descr] returns the underlying [Lwt_unix.file_descr] used to
        communicate over TCP. *)

    val peer : flow -> Unix.sockaddr
    (** [peer flow] retunrs the address of the peer connected to the given
        [flow]. *)

    val sock : flow -> Unix.sockaddr
    (** [sock flow] returns the current addres to which the socket is bound. *)
  end

  type configuration = { sockaddr : Lwt_unix.sockaddr; capacity : int }

  module Service :
    SERVICE
      with type configuration = configuration
       and type t = Lwt_unix.file_descr
       and type flow = Protocol.flow
       and type error =
            [ `Address_is_protected of Unix.sockaddr
            | `Operation_not_permitted of Unix.sockaddr
            | `Address_already_in_use of Unix.sockaddr
            | `Address_is_not_valid of Unix.sockaddr
            | `Cannot_assign_requested_address of Unix.sockaddr
            | `Bad_address
            | `Too_many_symbolic_links of Unix.sockaddr
            | `Name_too_long of Unix.sockaddr
            | `Operation_not_supported
            | `Limit_reached
            | `Protocol_error
            | `Firewall_rules_forbid_connection ]

  val protocol : (Lwt_unix.sockaddr, Protocol.flow) protocol

  type flow += T of Protocol.flow

  val service : (configuration, Service.t, Protocol.flow) service

  val configuration : ?capacity:int -> Lwt_unix.sockaddr -> configuration

  val resolve : port:int -> Lwt_unix.sockaddr resolver
end
