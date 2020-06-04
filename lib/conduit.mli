module Sigs = Sigs

type ('a, 'b) refl = Refl : ('a, 'a) refl

type resolvers

val empty : resolvers

type ('edn, 'flow) value = Value : 'flow -> ('edn, 'flow) value

module type S = sig
  type input

  type output

  type +'a s

  type scheduler

  module Client : sig
    module type PROTOCOL =
      Sigs.PROTOCOL
        with type input = input
         and type output = output
         and type +'a s = 'a s

    module type FLOW =
      Sigs.FLOW
        with type input = input
         and type output = output
         and type +'a s = 'a s

    type ('edn, 'flow) impl =
      (module PROTOCOL with type endpoint = 'edn and type flow = 'flow)

    type 'edn resolver = [ `host ] Domain_name.t -> 'edn option s

    type flow = private ..

    type ('edn, 'flow) protocol

    type error = [ `Msg of string | `Not_found ]

    val pp_error : error Fmt.t

    val recv :
      flow -> input -> ([ `Input of int | `End_of_input ], [> error ]) result s

    val send : flow -> output -> (int, [> error ]) result s

    val close : flow -> (unit, [> error ]) result s

    val register : protocol:('edn, 'flow) impl -> ('edn, 'flow) protocol

    module type REPR = sig
      type t

      type flow += T of t
    end

    val repr :
      ('edn, 'v) protocol -> (module REPR with type t = ('edn, 'v) value)

    val add :
      ('edn, _) protocol ->
      ?priority:int ->
      'edn resolver ->
      resolvers ->
      resolvers

    val abstract : (_, 'v) protocol -> 'v -> flow

    val connect :
      resolvers ->
      ?protocol:('edn, 'v) protocol ->
      [ `host ] Domain_name.t ->
      (flow, [> error ]) result s

    val impl_of_protocol :
      ('edn, 'flow) protocol ->
      (module PROTOCOL with type endpoint = 'edn and type flow = 'flow)

    val impl_of_flow :
      (_, 'flow) protocol -> (module FLOW with type flow = 'flow)

    val is : flow -> (_, 'flow) protocol -> 'flow option
  end

  module Service : sig
    module type SERVICE = Sigs.SERVICE with type +'a s = 'a s

    type ('cfg, 't, 'flow) impl =
      (module SERVICE
         with type configuration = 'cfg
          and type t = 't
          and type flow = 'flow)

    type 'flow protocol =
      | Protocol : (_, 'flow) Client.protocol -> 'flow protocol

    type ('cfg, 'v) service

    val register :
      service:('cfg, 't, 'flow) impl ->
      protocol:(_, 'flow) Client.protocol ->
      ('cfg, 't * 'flow) service

    type error = [ `Msg of string ]

    val pp_error : error Fmt.t

    val serve :
      'cfg ->
      service:('cfg, 't * 'flow) service ->
      ('t * 'flow protocol, [> error ]) result s

    val impl :
      ('cfg, 't * 'flow) service ->
      (module SERVICE
         with type configuration = 'cfg
          and type t = 't
          and type flow = 'flow)
  end
end

module Make
    (Scheduler : Sigs.SCHEDULER)
    (Input : Sigs.SINGLETON)
    (Output : Sigs.SINGLETON) :
  S
    with type input = Input.t
     and type output = Output.t
     and type +'a s = 'a Scheduler.t
