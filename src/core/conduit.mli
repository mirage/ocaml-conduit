module Endpoint = Endpoint

type resolvers
(** Type for resolvers map. *)

val empty : resolvers
(** [empty] is an empty {!resolvers} map. *)

module type S = Conduit_intf.S
(** @inline *)

module type IO = Conduit_intf.IO
(** @inline *)

module type BUFFER = Conduit_intf.BUFFER
(** @inline *)

module Make (IO : IO) (Input : BUFFER) (Output : BUFFER) :
  S
    with type input = Input.t
     and type output = Output.t
     and type +'a io = 'a IO.t
