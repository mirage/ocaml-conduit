type ('a, 'b) refl = Refl : ('a, 'a) refl

module type S1 = sig
  type 'a t
end

module Make (Key : S1) : sig
  (* XXX(dinosaure): only on [>= 4.06.0] *)
  type t = private ..

  type _ id = private ..

  module type S = sig
    type x

    type t += T of x

    type _ id += Id : x id

    val witness : x Key.t
  end

  type 'a s = (module S with type x = 'a)

  type v = Value : 'a * 'a Key.t -> v

  type k = Key : 'a Key.t * ('a -> t) -> k

  val equal : 'a s -> 'b s -> ('a, 'b) refl option

  val inj : 'a Key.t -> 'a s

  val prj : t -> v

  val extract : t -> 'a s -> 'a option

  val bindings : unit -> k list
end
