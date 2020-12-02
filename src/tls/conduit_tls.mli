module type IO = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  val fail : exn -> 'a t
end

module Make
    (IO : IO)
    (Conduit : Conduit.S
                 with type input = Cstruct.t
                  and type output = Cstruct.t
                  and type +'a io = 'a IO.t) : sig
  type 'flow protocol_with_tls

  val underlying : 'flow protocol_with_tls -> 'flow
  (** [underlying flow] returns underlying flow used by the TLS flow. *)

  val handshake : 'flow protocol_with_tls -> bool
  (** [handshake flow] returns [true] if {i handshake} is processing. *)

  val protocol_with_tls :
    ?host_of_endpoint:('edn -> string option) ->
    ('edn, 'flow) Conduit.protocol ->
    ('edn * Tls.Config.client, 'flow protocol_with_tls) Conduit.protocol
  (** From a given protocol [witness], it creates a new {i witness} of the
      protocol layered with TLS. *)

  type 'service service_with_tls

  val service_with_tls :
    ('cfg, 't, 'flow) Conduit.Service.t ->
    ('edn, 'flow) Conduit.protocol ->
    ('edn * Tls.Config.client, 'flow protocol_with_tls) Conduit.protocol ->
    ( 'cfg * Tls.Config.server,
      't service_with_tls,
      'flow protocol_with_tls )
    Conduit.Service.t
end
