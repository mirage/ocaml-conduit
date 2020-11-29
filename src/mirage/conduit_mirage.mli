module IO : Conduit.IO with type +'a t = 'a Lwt.t

include
  Conduit.S
    with type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a io = 'a Lwt.t

val serve :
  handler:('flow -> unit Lwt.t) ->
  ('cfg, _, 'flow) Service.t ->
  'cfg ->
  unit Lwt_condition.t * unit Lwt.t
