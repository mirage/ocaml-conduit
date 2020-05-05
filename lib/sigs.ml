type kind = UDP | TCP

type description = { name : string; port : int; kind : kind }

type 'x or_end_of_input = [ `End_of_input | `Input of 'x ]

module type FUNCTOR = sig
  type 'a t
end

module type SINGLETON = sig
  type t
end

type (+'a, 's) app

type 's scheduler = {
  bind : 'a 'b. ('a, 's) app -> ('a -> ('b, 's) app) -> ('b, 's) app;
  return : 'a. 'a -> ('a, 's) app;
}

module type BIJECTION = sig
  type +'a s

  type t

  external inj : 'a s -> ('a, t) app = "%identity"

  external prj : ('a, t) app -> 'a s = "%identity"
end

module Higher (Functor : sig
  type +'a t
end) : BIJECTION with type +'a s = 'a Functor.t = struct
  type +'a s = 'a Functor.t

  type t

  external inj : 'a s -> ('a, t) app = "%identity"

  external prj : ('a, t) app -> 'a s = "%identity"
end

module type SCHEDULER = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t
end

module type FLOW = sig
  type +'a s

  type flow

  type error

  type input

  and output

  val pp_error : error Fmt.t

  val recv : flow -> input -> (int or_end_of_input, error) result s

  val send : flow -> output -> (int, error) result s

  val close : flow -> (unit, error) result s
end

module type PROTOCOL = sig
  include FLOW

  type endpoint

  val flow : endpoint -> (flow, error) result s
end

module type SERVICE = sig
  type +'a s

  type flow

  type t

  type error

  type endpoint

  val make : endpoint -> (t, error) result s

  val pp_error : error Fmt.t

  val accept : t -> (flow, error) result s

  val close : t -> (unit, error) result s
end
