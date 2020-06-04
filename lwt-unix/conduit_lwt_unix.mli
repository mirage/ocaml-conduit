module Lwt_scheduler : Conduit.Sigs.SCHEDULER with type +'a t = 'a Lwt.t

include
  Conduit.S
    with type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a s = 'a Lwt.t
     and type scheduler = Conduit_lwt.scheduler
     and type ('edn, 'flow) Client.protocol =
          ('edn, 'flow) Conduit_lwt.Client.protocol
     and type ('cfg, 'v) Service.service =
          ('cfg, 'v) Conduit_lwt.Service.service
     and type Client.flow = Conduit_lwt.Client.flow

val serve_with_handler :
  handler:('flow Service.protocol -> 'flow -> unit Lwt.t) ->
  service:('cfg, 'master * 'flow) Service.service ->
  'cfg ->
  unit Lwt_condition.t * unit Lwt.t

val io_of_flow :
  Client.flow -> Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel
