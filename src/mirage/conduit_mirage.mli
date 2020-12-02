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

(** {3 [Mirage_flow.S].}

    Conduit does not follow the interface [Mirage_flow.S] but it provides its
    own {!Conduit.S.PROTOCOL} interface. A {i glue} is needed to pass from
    one to another and {i vice-versa}.

    This part of [Conduit_mirage] provides 2 ways:
    - [Make0] to produce a {!Conduit.S.PROTOCOL} from a [Mirage_flow.S] plus
      its [connect] function
    - [Make1] to produce a [Mirage_flow.S] plus its [connect] function from
      a {!Conduit.S.PROTOCOL}

    {b NOTE}:Both {i temporize} the flow by an intermediate buffer.

    We ensure that [Make0.{recv,send}] do one and unique call to
    [Mirage_flow.S.{read,write}].

    We ensure that [Make1.{read,write}] do one and unique call to
    [Conduit.S.PROTOCOL.{recv,send}] - despite {!Conduit_mirage_flow] which
    does several call of {!Conduit.S.PROTOCOL.send} until it sent all bytes. *)

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

val protocol_of_mirage_flow :
  (module Mirage_protocol with type flow = 'flow and type endpoint = 'edn) ->
  ('edn, 'flow flow0) protocol

module Make1
    (Protocol : Conduit.Conduit_intf.PROTOCOL
                  with type 'a io = 'a Lwt.t
                   and type input = Cstruct.t
                   and type output = Cstruct.t) : sig
  include Mirage_flow.S with type flow = Protocol.flow flow1

  type endpoint = Protocol.endpoint

  val connect : endpoint -> (flow, error) result Lwt.t
end
