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
    [write] - you should be aware about that more specially when you use
    [Conduit_tls] or [Conduit_lwt_ssl].

    Due to a specific behavior, [Lwt_io] does not fit with some specific
    protocols - non thread-safe protocols, {i send-first} protocols, etc. From
    these reasons, and even if {!TCP} try to the best to fit under an [Lwt_io],
    you should not use this function. *)

type ('a, 'b, 'c) service = ('a, 'b, 'c) Service.t
(** The type for lwt services. *)

val serve :
  ?timeout:int ->
  handler:(flow -> unit Lwt.t) ->
  service:('cfg, 'service, 'flow) service ->
  'cfg ->
  unit Lwt_condition.t * (unit -> unit Lwt.t)
(** [serve ~handler ~service cfg] creates an usual infinite [service] loop from
    the given configuration ['cfg]. It returns the {i promise} to launch the
    loop and a condition variable to stop the loop.

    {[
      let stop, loop = serve ~handler ~service:TCP.service cfg in
      Lwt.both
        ( Lwt_unix.sleep 10. >>= fun () ->
          Lwt_condition.broadcast stop () ;
          Lwt.return () )
        (loop ())
    ]}

    In your example, we want to launch a server only for 10 seconds. To help the
    user, the option [?timeout] allows us to wait less than [timeout] seconds. *)

module TCP : sig
  (** Implementation of TCP protocol as a client.

      Behaviours of [Protocol] differs from {i syscall} provided by [Lwt_unix].
      This is a description of what they currently do.

      {b NOTE}: [recv] wants to fill the given buffer as much as possible until
      it has reached {i end-of-input}. In other words, [recv] can do a multiple
      call to [Lwt_unix.recv] to fill the given buffer.

      {b NOTE}: [send] tries to send as much as it can the given buffer.
      However, if internal call of [Lwt_unix.send] returns something smaller
      than what we requested, we stop the process and return how many byte(s) we
      sended. In other word, [send] can do a multiple call to [Lwt_unix.send]
      until we fully sended what we wanted. *)

  type error =
    [ `Closed_by_peer
    | `Address_already_in_use of Unix.sockaddr
    | `Cannot_assign_requested_address of Unix.sockaddr
    | `Address_family_not_supported_by_protocol of Unix.sockaddr
    | `Operation_already_in_progress
    | `Bad_address
    | `Network_is_unreachable
    | `Connection_timed_out
    | `Connection_refused
    | `Transport_endpoint_is_not_connected
    | `Address_is_protected of Unix.sockaddr
    | `Operation_not_permitted of Unix.sockaddr
    | `Address_is_not_valid of Unix.sockaddr
    | `Too_many_symbolic_links of Unix.sockaddr
    | `Name_too_long of Unix.sockaddr
    | `Operation_not_supported
    | `Limit_reached
    | `Protocol_error
    | `Firewall_rules_forbid_connection ]

  module Protocol : sig
    include
      PROTOCOL with type endpoint = Lwt_unix.sockaddr and type error = error

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
       and type error = error

  val flow : Protocol.flow t

  val protocol : (Lwt_unix.sockaddr, Protocol.flow) protocol

  type flow += T of Protocol.flow

  val service : (configuration, Service.t, Protocol.flow) service

  val resolve : port:int -> Lwt_unix.sockaddr resolver
end
