open Conduit_mirage

type ('stack, 'ip) endpoint = {
  stack : 'stack;
  keepalive : Mirage_protocols.Keepalive.t option;
  ip : 'ip;
  port : int;
}

type 'stack configuration = {
  stack : 'stack;
  keepalive : Mirage_protocols.Keepalive.t option;
  port : int;
}

module Make (StackV4 : Mirage_stack.V4) : sig
  type protocol

  val protocol :
    ((StackV4.t, Ipaddr.V4.t) endpoint, protocol) Conduit_mirage.protocol

  val dst : protocol -> Ipaddr.V4.t * int

  type Conduit_mirage.flow += T of protocol

  type service

  val service : (StackV4.t configuration, service, protocol) Service.t

  val configuration :
    StackV4.t ->
    ?keepalive:Mirage_protocols.Keepalive.t ->
    port:int ->
    StackV4.t configuration
end
