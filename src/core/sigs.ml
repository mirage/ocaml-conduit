type 'x or_end_of_flow = [ `End_of_flow | `Input of 'x ]

module type FLOW = sig
  (** [FLOW] is the signature for flow clients.

      A [flow] is an abstract value over which I/O functions such as {!send},
      {!recv} and {!close} can be used.

      {[
        type input = bytes

        and output = string

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
      can be wrapped into a TLS layer. However, the goal is to be able to
      implement a protocol without such complexity. *)

  type +'a io

  type flow

  (** {3 Input & Output.}

      Depending on the I/O model, the type for inputs and outputs can differ ;
      for instance they could allow users the ability to define capabilities on
      them such as {i read} or {i write} capabilities.

      However, in most of the current [Conduit] backends:

      {[
        type input = Cstruct.t

        type output = Cstruct.t
      ]} *)

  type input

  and output

  (** {3 Errors.} *)

  type error
  (** The type for errors. *)

  val pp_error : error Fmt.t
  (** [pp_error] is the pretty-printer for {!error}. *)

  val recv : flow -> input -> (int or_end_of_flow, error) result io
  (** [recv flow input] is [Ok (`Input len)] iff [len] bytes of data has been
      received from the flow [flow] and copied in [input]. *)

  val send : flow -> output -> (int, error) result io
  (** [send t output] is [Ok len] iff [len] bytes of data from [output] has been
      sent over the flow [flow]. *)

  val close : flow -> (unit, error) result io
  (** [close flow] closes [flow]. Subsequent calls to {!recv} on [flow] will
      return [`End_of_flow]. Subsequent calls to {!send} on [t] will return an
      [Error]. *)
end

module type PROTOCOL = sig
  include FLOW

  type endpoint

  val connect : endpoint -> (flow, error) result io
end

module type SERVICE = sig
  include FLOW

  type t

  type configuration

  val init : configuration -> (t, error) result io

  val accept : t -> (flow, error) result io

  val stop : t -> (unit, error) result io
end

module type IO = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t
end

module type BUFFER = sig
  type t
end
