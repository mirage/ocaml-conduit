(** Common TLS implementation with Conduit.

    The current implementation of the TLS layer over an underlying protocol
    respects some assumptions and it has a specific behaviour which is decribed
    here:

    The {i handshake} is not done when we initialize the flow. Only a call to
    [recv] or [send] really starts the handshake with your peer. In that
    context, a concurrent call of these actions should put some trouble into the
    handshake and they must be protected by an exclusion.

    In other words due to the non-atomicity of [recv] and [send], while the
    handshake, you should ensure to finish a call of one of them before to call
    the other. A mutex should be used in this context to protect the mutual
    exclusion between [recv] and [send]. In others words, such process is safe:

    {[
      let* _ = Conduit.send tls_flow raw in
      let* _ = Conduit.recv tls_flow raw in
    ]}

    Where such process is not safe:

    {[
      async (fun () -> Conduit.send tls_flow raw) ;
      async (fun () -> Conduit.recv tls_flow raw)
    ]}

    The non-atomicity of [send] and [recv] is due to the underlying handshake of
    TLS which can appear everytime. By this fact, [send] or [recv] (depends
    which is executed first) can start an handshake process which can call
    several times underlying [Flow.send] and [Flow.recv] processes (no 0-RTT).
    If you use [async], the scheduler can misleading/misorder handshake started
    with one to the other call to [send] and [recv].

    A solution such as a {i mutex} to ensure the exclusivity between [send] and
    [recv] can be used - it does not exists at this layer where such abstraction
    is not available.

    This design appear when you use [LWT] or [ASYNC] which can do a concurrence
    between {i promises}. Without such {i scheduler}, the process is sequential
    and the OCaml {i scheduler} should not re-order sub-processes of
    [Conduit.send] and [Conduit.recv]. *)

module Make
    (IO : Conduit.IO)
    (Conduit : Conduit.S
                 with type input = Cstruct.t
                  and type output = Cstruct.t
                  and type +'a io = 'a IO.t) : sig
  type 'flow with_tls

  val underlying : 'flow with_tls -> 'flow
  (** [underlying flow] returns underlying flow used by the TLS flow. *)

  val handshake : 'flow with_tls -> bool
  (** [handshake flow] returns [true] if {i handshake} is processing. *)

  val flow_with_tls : 'flow Conduit.t -> 'flow with_tls Conduit.t

  val protocol_with_tls :
    'flow with_tls Conduit.t ->
    ('edn, 'flow) Conduit.protocol ->
    ('edn * Tls.Config.client, 'flow with_tls) Conduit.protocol
  (** From a given protocol [witness], it creates a new {i witness} of the
      protocol layered with TLS. *)

  type 'service service_with_tls

  val service_with_tls :
    'flow with_tls Conduit.t ->
    ('cfg, 't, 'flow) Conduit.Service.t ->
    ( 'cfg * Tls.Config.server,
      't service_with_tls,
      'flow with_tls )
    Conduit.Service.t
end
