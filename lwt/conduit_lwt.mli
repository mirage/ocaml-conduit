(** Conduit with LWT. *)

module Lwt_scheduler : Conduit.Sigs.SCHEDULER with type +'a t = 'a Lwt.t

include
  Conduit.S
    with type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a s = 'a Lwt.t

val serve_with_handler :
  handler:('flow Service.protocol -> 'flow -> unit Lwt.t) ->
  service:('cfg, 'master * 'flow) Service.service ->
  'cfg ->
  unit Lwt_condition.t * unit Lwt.t

(** Common interface to properly expose a protocol.

    If a protocol wants to be fully-compatible with [conduit],
    it should expose such implementation which is an aggregate
    of {i types witnesses}.

    At least, [endpoint], [configuration] and [service] must be
    exposed to be usable by the end-user. Otherwise, the given
    protocol can not be:
    {ul
    {- registered into {!resolvers}}
    {- used as a service with {!serve_with_handler]/{!serve}}}

    [protocol] can be hidden - but must be registered with
    {!register_protocol}. However, in such case, the end-user
    will not be able to {i destruct} (with {!is}/{!Witness.equal_protocol})
    the given {i flow} to the underlying concrete value.
*)

module type CONDUIT = sig
  type endpoint

  type flow

  type configuration

  type master

  val protocol : (endpoint, flow) Client.protocol

  val service : (configuration, master * flow) Service.service
end
