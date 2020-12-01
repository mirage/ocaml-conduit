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

val page_size : int

module type Mirage_protocol = sig
  include Mirage_flow.S

  type endpoint

  val connect : endpoint -> (flow, error) result Lwt.t
end

type 'flow flow0 = {
  flow : 'flow;
  queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
}

type 'flow flow1 = {
  flow : 'flow;
  linger : Cstruct.t;
  queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
}

module Make0 (Protocol : Mirage_protocol) :
  Conduit.Conduit_intf.PROTOCOL
    with type endpoint = Protocol.endpoint
     and type input = Cstruct.t
     and type output = Cstruct.t
     and type +'a io = 'a Lwt.t
     and type flow = Protocol.flow flow0

module Make1
    (Protocol : Conduit.Conduit_intf.PROTOCOL
                  with type 'a io = 'a Lwt.t
                   and type input = Cstruct.t
                   and type output = Cstruct.t) : sig
  include Mirage_flow.S with type flow = Protocol.flow flow1

  type endpoint = Protocol.endpoint

  val connect : endpoint -> (flow, error) result Lwt.t
end
