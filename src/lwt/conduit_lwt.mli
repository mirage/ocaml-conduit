module IO : Conduit.IO with type +'a t = 'a Lwt.t

include
  Conduit.S
    with type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a io = 'a Lwt.t

val io_of_flow :
  flow -> Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel

val serve :
  handler:('flow -> unit Lwt.t) ->
  service:('cfg, 'service, 'flow) Service.service ->
  'cfg ->
  unit Lwt_condition.t * unit Lwt.t
(** [serve ~handler ~service cfg] creates an usual infinite [service]
    loop from the given configuration ['cfg]. It returns the {i promise} to launch
    the loop and a condition variable to stop the loop.

    {[
      let stop, loop = serve_with_handle
        ~handler ~service:TCP.service cfg in
      Lwt.both
        (Lwt_unix.sleep 10. >>= fun () ->
         Lwt_condition.broadcast stop () ;
         Lwt.return ())
        loop
    ]}

    In your example, we want to launch a server only for 10 seconds. *)

(** Common interface to properly expose a protocol.

    If a protocol wants to be fully-compatible with [conduit],
    it should expose such implementation which is an aggregate
    of {i types witnesses}.
*)

module TCP : sig
  (** Implementation of TCP protocol as a client.

      Behaviours of [Protocol] differs from {i syscall} provided by [Lwt_unix].
      This is a description of what they currently do.

      {b NOTE}: [recv] wants to fill the given buffer as much as possible until it
      has reached {i end-of-input}. In other words, [recv] can do a multiple call
      to [Lwt_unix.recv] to fill the given buffer.

      {b NOTE}: [send] tries to send as much as it can the given buffer. However,
      if internal call of [Lwt_unix.send] returns something smaller than what we
      requested, we stop the process and return how many byte(s) we sended. In
      other word, [send] can do a multiple call to [Lwt_unix.send] until we fully
      sended what we wanted. *)

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
    (** [peer flow] retunrs the address of the peer connected to the given [flow]. *)

    val sock : flow -> Unix.sockaddr
    (** [sock flow] returns the current addres to which the socket is bound. *)
  end

  type configuration = { sockaddr : Lwt_unix.sockaddr; capacity : int }

  module Server :
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

  type t = (Lwt_unix.sockaddr, Protocol.flow) Conduit.value

  type flow += T of t

  val service : (configuration, Server.t, Protocol.flow) Service.service

  val resolv_conf : port:int -> Lwt_unix.sockaddr resolver
end
