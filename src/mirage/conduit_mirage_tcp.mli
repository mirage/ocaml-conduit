open Conduit_mirage

type ('stack, 'ip) endpoint = {
  stack : 'stack;
  keepalive : Mirage_protocols.Keepalive.t option;
  nodelay : bool;
  ip : 'ip;
  port : int;
}

type 'stack configuration = {
  stack : 'stack;
  keepalive : Mirage_protocols.Keepalive.t option;
  nodelay : bool;
  port : int;
}

module Make (StackV4 : Mirage_stack.V4) : sig
  type flow

  val flow : flow t

  val protocol :
    ((StackV4.t, Ipaddr.V4.t) endpoint, flow) Conduit_mirage.protocol

  val dst : flow -> Ipaddr.V4.t * int

  type service

  val service : (StackV4.t configuration, service, flow) Service.t
end
