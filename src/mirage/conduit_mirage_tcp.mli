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

module Make (Stack : Mirage_stack.V4V6) : sig
  type protocol

  val protocol :
    ((Stack.t, Ipaddr.t) endpoint, protocol) Conduit_mirage.protocol

  val dst : protocol -> Ipaddr.t * int

  type service

  val service : (Stack.t configuration, service, protocol) Service.service
end
