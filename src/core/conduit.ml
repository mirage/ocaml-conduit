module Sigs = Sigs

type ('a, 'b) refl = Refl : ('a, 'a) refl

let strf = Format.asprintf

type _ witness = ..

type (+'a, 's) app

type _ resolver =
  | Resolver : {
      priority : int option;
      resolve : [ `host ] Domain_name.t -> ('edn option, 's) app;
      witness : 's witness;
    }
      -> ('edn * 's) resolver

type ('a, 'b) value = Value : 'b -> ('a, 'b) value

let reword_error f = function Ok x -> Ok x | Error err -> Error (f err)

let msgf fmt = Fmt.kstrf (fun err -> `Msg err) fmt

[@@@warning "-37"]

type ('a, 'b, 'c) thd =
  | Thd : 'b -> ('a, 'b, 'c) thd
      (** XXX(dinosaure): we must define [(_, _, _) thd] to be able to keep some
          existential types (eg. ['cfg] and ['flow] when we use [('cfg, 't, 'flow)
          service]) but still to use only on (eg. ['t]).

          We add [warning "-37"] to be able to compile the project. *)

let error_msgf fmt = Format.kasprintf (fun err -> Error (`Msg err)) fmt

module Map =
  E1.Make
    (struct
      type _ t = string
    end)
    (struct
      type 'a t = 'a resolver
    end)

type resolvers = Map.t

let empty = Map.empty

module type S = sig
  type input

  type output

  type +'a io

  type scheduler

  type flow = private ..

  type error = [ `Msg of string | `Not_found ]

  val pp_error : error Fmt.t

  val recv :
    flow -> input -> ([ `Input of int | `End_of_flow ], [> error ]) result io

  val send : flow -> output -> (int, [> error ]) result io

  val close : flow -> (unit, [> error ]) result io

  module type FLOW =
    Sigs.FLOW
      with type input = input
       and type output = output
       and type +'a io = 'a io

  module type PROTOCOL =
    Sigs.PROTOCOL
      with type input = input
       and type output = output
       and type +'a io = 'a io

  type ('edn, 'flow) impl =
    (module PROTOCOL with type endpoint = 'edn and type flow = 'flow)

  type ('edn, 'flow) protocol

  val register : protocol:('edn, 'flow) impl -> ('edn, 'flow) protocol

  module type REPR = sig
    type t

    type flow += T of t
  end

  val repr : ('edn, 'v) protocol -> (module REPR with type t = ('edn, 'v) value)

  type unpack = Flow : 'flow * (module FLOW with type flow = 'flow) -> unpack

  val unpack : flow -> unpack

  val impl :
    ('edn, 'flow) protocol ->
    (module PROTOCOL with type endpoint = 'edn and type flow = 'flow)

  val cast : flow -> ('edn, 'flow) protocol -> 'flow option

  val pack : ('edn, 'v) protocol -> 'v -> flow

  type 'edn resolver = [ `host ] Domain_name.t -> 'edn option io

  type nonrec resolvers = resolvers

  val empty : resolvers

  val add :
    ('edn, 'flow) protocol ->
    ?priority:int ->
    'edn resolver ->
    resolvers ->
    resolvers

  val resolve :
    resolvers ->
    ?protocol:('edn, 'v) protocol ->
    [ `host ] Domain_name.t ->
    (flow, [> error ]) result io

  val connect : 'edn -> ('edn, _) protocol -> (flow, [> error ]) result io

  module type SERVICE = Sigs.SERVICE with type +'a io = 'a io

  module Service : sig
    type ('cfg, 't, 'flow) impl =
      (module SERVICE
         with type configuration = 'cfg
          and type t = 't
          and type flow = 'flow)

    type ('cfg, 't, 'flow) service

    val equal :
      ('cfg0, 't0, 'flow0) service ->
      ('cfg1, 't1, 'flow1) service ->
      (('cfg0, 'cfg1) refl * ('t0, 't1) refl * ('flow0, 'flow1) refl) option

    val register : service:('cfg, 't, 'flow) impl -> ('cfg, 't, 'flow) service

    type error = [ `Msg of string ]

    val pp_error : error Fmt.t

    val init :
      'cfg -> service:('cfg, 't, 'flow) service -> ('t, [> error ]) result io

    val accept :
      service:('cfg, 't, 'flow) service -> 't -> ('flow, [> error ]) result io

    val close :
      service:('cfg, 't, 'flow) service -> 't -> (unit, [> error ]) result io

    val impl :
      ('cfg, 't, 'flow) service ->
      (module SERVICE
         with type configuration = 'cfg
          and type t = 't
          and type flow = 'flow)
  end
end

module type IO = Sigs.IO

module type BUFFER = Sigs.BUFFER

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

module Make (IO : IO) (Input : BUFFER) (Output : BUFFER) :
  S
    with type input = Input.t
     and type output = Output.t
     and type +'a io = 'a IO.t = struct
  module Bijection = Higher (IO)

  type scheduler = Bijection.t

  let inj = Bijection.inj

  let prj = Bijection.prj

  let return = IO.return

  let ( >>= ) x f = IO.bind x f

  let ( >>| ) x f = x >>= fun x -> return (f x)

  let ( >>? ) x f =
    x >>= function Ok x -> f x | Error err -> return (Error err)

  type +'a io = 'a IO.t

  type _ witness += Witness : scheduler witness

  let witness : scheduler witness = Witness

  type input = Input.t

  type output = Output.t

  module type PROTOCOL =
    Sigs.PROTOCOL
      with type input = input
       and type output = output
       and type +'a io = 'a io

  module type FLOW =
    Sigs.FLOW
      with type input = input
       and type output = output
       and type +'a io = 'a io

  type ('edn, 'flow) impl =
    (module PROTOCOL with type endpoint = 'edn and type flow = 'flow)

  type 'edn key = ('edn * scheduler) Map.key

  type 'edn resolver = [ `host ] Domain_name.t -> 'edn option io

  module F = struct
    type _ t =
      | Protocol : 'edn key * ('edn, 'flow) impl -> ('edn, 'flow) value t
  end

  module Ptr = E0.Make (F)

  type flow = Ptr.t = private ..

  type ('edn, 'flow) protocol = ('edn, 'flow) value Ptr.s

  (* XXX(dinosaure): note about performance, [Ptr.prj] can cost where
   * it's a lookup into the global [hashtbl] (created by [Ptr]). However,
   * the usual pattern of [Conduit] is multiple calls of [send]/[recv] with
   * the same [flow].
   *
   * Implementation of internal [hashtbl] memoize such case. We have different
   * overheads:
   * - about [recv]/[send], it's around ~500ns (first call), ~125ns (subsequent calls)
   * - about [flow] & [Flow.recv]/[Flow.send], it's aroung ~75ns
   *
   * However, keep in your mind that:
   * - the internal [hashtbl] should be small (smaller than 16 elements)
   * - performance is intrinsic with [caml_hash]
   *)

  let recv flow input =
    let (Ptr.Value (flow, Protocol (_, (module Protocol))) : Ptr.v) =
      Ptr.prj flow in
    let (Value flow) = flow in
    Protocol.recv flow input >>| function
    | Ok _ as v -> v
    | Error err -> Error (`Msg (strf "%a" Protocol.pp_error err))

  let send (flow : Ptr.t) output =
    let (Ptr.Value (flow, Protocol (_, (module Protocol))) : Ptr.v) =
      Ptr.prj flow in
    let (Value flow) = flow in
    Protocol.send flow output >>| function
    | Ok _ as v -> v
    | Error err -> Error (`Msg (strf "%a" Protocol.pp_error err))

  let close flow =
    let (Ptr.Value (flow, Protocol (_, (module Protocol)))) = Ptr.prj flow in
    let (Value flow) = flow in
    Protocol.close flow >>| function
    | Ok _ as v -> v
    | Error err -> Error (`Msg (strf "%a" Protocol.pp_error err))

  let register :
      type edn flow. protocol:(edn, flow) impl -> (edn, flow) protocol =
   fun ~protocol ->
    let key = Map.Key.create "" in
    Ptr.inj (Protocol (key, protocol))

  module type REPR = sig
    type t

    type flow += T of t
  end

  let repr :
      type edn v.
      (edn, v) protocol -> (module REPR with type t = (edn, v) value) =
   fun (module Witness) ->
    let module M = struct
      include Witness

      type t = x
    end in
    (module M)

  let ( <.> ) f g x = f (g x)

  type nonrec resolvers = resolvers

  let empty = empty

  let add :
      type edn flow.
      (edn, flow) protocol ->
      ?priority:int ->
      edn resolver ->
      resolvers ->
      resolvers =
   fun (module Witness) ?priority resolve ->
    let (Protocol (key, _)) = Witness.witness in
    let resolve = inj <.> resolve in
    Map.add key (Resolver { priority; resolve; witness })

  type error = [ `Msg of string | `Not_found ]

  let pf ppf fmt = Format.fprintf ppf fmt

  let pp_error ppf = function
    | `Msg err -> pf ppf "%s" err
    | `Not_found -> pf ppf "Not found"

  let flow_of_endpoint :
      type edn. edn key -> edn -> (flow, [> error ]) result io =
   fun key edn ->
    let rec go = function
      | [] -> return (Error `Not_found)
      | Ptr.Key (Protocol (k, (module Protocol)), ctor) :: r ->
      match Map.Key.(key == k) with
      | None -> go r
      | Some E1.Refl.Refl -> (
          Protocol.connect edn >>= function
          | Ok flow -> return (Ok (ctor (Value flow)))
          | Error _err -> go r) in
    go (Ptr.bindings ())

  let flow_of_protocol :
      type edn flow. (edn, flow) protocol -> edn -> (flow, [> error ]) result io
      =
   fun (module Witness) edn ->
    let (Protocol (_, (module Protocol))) = Witness.witness in
    Protocol.connect edn >>= function
    | Ok flow -> return (Ok flow)
    | Error err -> return (error_msgf "%a" Protocol.pp_error err)

  type endpoint = Endpoint : 'edn key * 'edn -> endpoint

  module Refl = struct
    type ('a, 'b) t = Refl : ('a, 'a) t
  end

  let scheduler : type s. s witness -> (s, scheduler) Refl.t option = function
    | Witness -> Some Refl.Refl
    | _ -> None

  let inf = -1

  and sup = 1

  let resolve : resolvers -> [ `host ] Domain_name.t -> endpoint list io =
   fun m domain_name ->
    let rec go acc = function
      | [] -> return (List.rev acc) (* XXX(dinosaure): keep order. *)
      | Map.Value (k, Resolver { resolve; witness; _ }) :: r ->
      match scheduler witness with
      | None -> go acc r
      | Some Refl.Refl -> (
          resolve domain_name |> prj >>= function
          | Some edn -> go (Endpoint (k, edn) :: acc) r
          | None -> go acc r) in
    let compare (Map.Value (_, Resolver { priority = pa; _ }))
        (Map.Value (_, Resolver { priority = pb; _ })) =
      match (pa, pb) with
      | Some a, Some b -> (Stdlib.compare : int -> int -> int) a b
      | None, Some _ -> sup
      | Some _, None -> inf
      | None, None -> 0 in
    go [] (List.sort compare (Map.bindings m))

  let create :
      resolvers -> [ `host ] Domain_name.t -> (flow, [> error ]) result io =
   fun m domain_name ->
    resolve m domain_name >>= fun l ->
    let rec go = function
      | [] -> return (Error `Not_found)
      | Endpoint (key, edn) :: r -> (
          flow_of_endpoint key edn >>= function
          | Ok flow -> return (Ok flow)
          | Error _err -> go r) in
    go l

  let pack : type edn v. (edn, v) protocol -> v -> flow =
   fun (module Witness) flow -> Witness.T (Value flow)

  let resolve :
      type edn v.
      resolvers ->
      ?protocol:(edn, v) protocol ->
      [ `host ] Domain_name.t ->
      (flow, [> error ]) result io =
   fun m ?protocol domain_name ->
    match protocol with
    | None -> create m domain_name
    | Some (module Witness) ->
        let (Protocol (key', _)) = Witness.witness in
        resolve m domain_name >>= fun l ->
        let rec go = function
          | [] -> return (Error `Not_found)
          | Endpoint (key, edn) :: r ->
          match Map.Key.(key == key') with
          | None -> go r
          | Some E1.Refl.Refl -> (
              flow_of_protocol (module Witness) edn >>= function
              | Ok flow -> return (Ok (Witness.T (Value flow)))
              | Error _err -> go r) in
        go l

  let connect :
      type edn v. edn -> (edn, v) protocol -> (flow, [> error ]) result io =
   fun edn (module Witness) ->
    let (Protocol (_, (module Protocol))) = Witness.witness in
    Protocol.connect edn >>| reword_error (msgf "%a" Protocol.pp_error)
    >>? fun flow -> return (Ok (Witness.T (Value flow)))

  type unpack = Flow : 'flow * (module FLOW with type flow = 'flow) -> unpack

  let unpack : flow -> unpack =
   fun flow ->
    let (Ptr.Value (flow, Protocol (_, (module Protocol))) : Ptr.v) =
      Ptr.prj flow in
    let (Value flow) = flow in
    Flow (flow, (module Protocol))

  let impl :
      type edn flow.
      (edn, flow) protocol ->
      (module PROTOCOL with type endpoint = edn and type flow = flow) =
   fun (module Witness) ->
    let (Protocol (_, (module Protocol))) = Witness.witness in
    (module Protocol)

  let cast : type edn v. flow -> (edn, v) protocol -> v option =
   fun flow witness ->
    match Ptr.extract flow witness with
    | Some (Value flow) -> Some flow
    | None -> None

  module type SERVICE = Sigs.SERVICE with type +'a io = 'a io

  module Service = struct
    type ('cfg, 't, 'flow) impl =
      (module SERVICE
         with type configuration = 'cfg
          and type t = 't
          and type flow = 'flow)

    module F = struct
      type 't t =
        | Service : 'cfg key * ('cfg, 't, 'flow) impl -> ('cfg, 't, 'flow) thd t
    end

    module Svc = E0.Make (F)

    type ('cfg, 't, 'flow) service = ('cfg, 't, 'flow) thd Svc.s

    let register :
        type cfg t flow. service:(cfg, t, flow) impl -> (cfg, t, flow) service =
     fun ~service ->
      let cfg = Map.Key.create "" in
      Svc.inj (Service (cfg, service))

    type error = [ `Msg of string ]

    let pp_error ppf = function `Msg err -> Fmt.string ppf err

    let equal :
        type a b c d e f.
        (a, b, c) service ->
        (d, e, f) service ->
        ((a, d) refl * (b, e) refl * (c, f) refl) option =
     fun (module A) (module B) ->
      match A.Id with B.Id -> Some (Refl, Refl, Refl) | _ -> None

    let init :
        type cfg t flow.
        cfg -> service:(cfg, t, flow) service -> (t, [> error ]) result io =
     fun edn ~service:(module Witness) ->
      let (Service (_, (module Service))) = Witness.witness in
      Service.init edn >>= function
      | Ok t -> return (Ok t)
      | Error err -> return (error_msgf "%a" Service.pp_error err)

    let accept :
        type cfg t flow.
        service:(cfg, t, flow) service -> t -> (flow, [> error ]) result io =
     fun ~service:(module Witness) t ->
      let (Service (_, (module Service))) = Witness.witness in
      Service.accept t >>= function
      | Ok flow -> return (Ok flow)
      | Error err -> return (error_msgf "%a" Service.pp_error err)

    let close :
        type cfg t flow.
        service:(cfg, t, flow) service -> t -> (unit, [> error ]) result io =
     fun ~service:(module Witness) t ->
      let (Service (_, (module Service))) = Witness.witness in
      Service.close t >>= function
      | Ok () -> return (Ok ())
      | Error err -> return (error_msgf "%a" Service.pp_error err)

    let impl :
        type cfg t flow.
        (cfg, t, flow) service ->
        (module SERVICE
           with type configuration = cfg
            and type t = t
            and type flow = flow) =
     fun (module S) ->
      let (Service (_, (module Service))) = S.witness in
      (module Service)
  end
end
