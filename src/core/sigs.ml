type 'x or_end_of_flow = [ `End_of_flow | `Input of 'x ]

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
  (** [FLOW] is the signature for flow clients.

      A [flow] is an abstract value over which I/O functions such as {!send},
     {!recv} and {!close} can be used.

      {[
        type input = bytes and output = string
        type +'a s = 'a

        let process flow =
          let buf = Bytes.create 0x1000 in
          match Flow.recv flow buf with
          | Ok (`Input len) ->
            let str = Bytes.sub_string buf 0 len in
            ignore (Flow.send flow str)
          | _ -> failwith "Flow.recv"
      ]}

      The given flow can be more complex than a simple TCP flow for example. It
     can be wrapped into a TLS layer. However, the goal is to be able to implement
     a protocol without such complexity.
  *)

  type +'a s

  type flow

  (** {3 Input & Output.}

      Depending on the I/O model, the type for inputs and outputs can differ ;
     for instance they could allow users the ability to define capabilities on
     them such as {i read} or {i write} capabilities.

      However, in most of the current [Conduit] backends:

      {[
        type input = Cstruct.t
        type output = Cstruct.t
      ]}
  *)

  type input

  and output

  (** {3 Errors.} *)

  type error
  (** The type for errors. *)

  val pp_error : error Fmt.t
  (** [pp_error] is the pretty-printer for {!error}. *)

  val recv : flow -> input -> (int or_end_of_flow, error) result s
  (** [recv flow input] is [Ok (`Input len)] iff [len] bytes of data has been received from
     the flow [flow] and copied in [input]. *)

  val send : flow -> output -> (int, error) result s
  (** [send t output] is [Ok len] iff [len] bytes of data from [output] has been
     sent over the flow [flow]. *)

  val close : flow -> (unit, error) result s
  (** [close flow] closes [flow]. Subsequent calls to {!recv} on [flow] will
     return [`End_of_flow]. Subsequent calls to {!send} on [t] will return an
     [Error]. *)
end

module type PROTOCOL = sig
  include FLOW

  type endpoint

  val connect : endpoint -> (flow, error) result s
end

module type SERVICE = sig
  type +'a s

  type flow

  type t

  type error

  type configuration

  val make : configuration -> (t, error) result s

  val pp_error : error Fmt.t

  val accept : t -> (flow, error) result s

  val close : t -> (unit, error) result s
end
