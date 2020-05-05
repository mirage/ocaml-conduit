module Mirage_scheduler : Conduit.Sigs.SCHEDULER with type +'a t = 'a Lwt.t

include
  Conduit.S
    with type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a s = 'a Lwt.t
     and type scheduler = Conduit_lwt.scheduler
     and type 'a key = ('a * Conduit_lwt.scheduler) Conduit.key
     and type 'a Witness.protocol = 'a Conduit_lwt.Witness.protocol
     and type 'a Witness.service = 'a Conduit_lwt.Witness.service
     and type flow = Conduit_lwt.flow

val serve_with_handler :
  handler:('flow Witness.protocol -> 'flow -> unit Lwt.t) ->
  key:'cfg key ->
  service:('master * 'flow) Witness.service ->
  'cfg ->
  unit Lwt_condition.t * unit Lwt.t
