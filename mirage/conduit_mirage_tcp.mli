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
  type protocol

  val protocol : ((StackV4.t, Ipaddr.V4.t) endpoint, protocol) Conduit_mirage.protocol

  val dst : protocol -> Ipaddr.V4.t * int

  type service

  val service : (StackV4.t configuration, service, protocol) Service.service
end
