module IO : Conduit.IO with type +'a t = 'a Lwt.t

include
  Conduit.S
    with type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a io = 'a Lwt.t

val serve :
  handler:('flow -> unit Lwt.t) ->
  service:('cfg, 'master, 'flow) Service.service ->
  'cfg ->
  unit Lwt_condition.t * unit Lwt.t

module type CONDUIT = sig
  type endpoint

  type flow

  type configuration

  type master

  val protocol : (endpoint, flow) protocol

  val service : (configuration, master, flow) Service.service
end
