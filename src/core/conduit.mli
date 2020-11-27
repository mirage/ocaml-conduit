module type S = sig
  include Conduit_impl.S
  (** @inline *)
end

module type IO = Sigs.IO
(** @inline *)

module type BUFFER = Sigs.BUFFER
(** @inline *)

module Make (IO : IO) (Input : BUFFER) (Output : BUFFER) :
  S
    with type input = Input.t
     and type output = Output.t
     and type +'a io = 'a IO.t
