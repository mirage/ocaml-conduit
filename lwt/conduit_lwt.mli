(** Conduit with LWT. *)

module Lwt_scheduler : Conduit.Sigs.SCHEDULER with type +'a t = 'a Lwt.t

include
  Conduit.S
    with type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a s = 'a Lwt.t

val serve_with_handler :
  handler:('flow Witness.protocol -> 'flow -> unit Lwt.t) ->
  key:'cfg key ->
  service:('master * 'flow) Witness.service ->
  'cfg ->
  unit Lwt_condition.t * unit Lwt.t
