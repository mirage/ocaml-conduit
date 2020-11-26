module Endpoint = Endpoint
module Sigs = Sigs

type ('a, 'b) refl = Refl : ('a, 'a) refl

let strf = Format.asprintf

type _ witness = ..

type (+'a, 's) app

type _ resolver =
  | Resolver : {
      priority : int option;
      resolve : Endpoint.t -> ('edn option, 's) app;
      witness : 's witness;
    }
      -> ('edn * 's) resolver

let reword_error f = function Ok x -> Ok x | Error err -> Error (f err)

let msgf fmt = Fmt.kstrf (fun err -> `Msg err) fmt

let src = Logs.Src.create "conduit"

module Log = (val Logs.src_log src : Logs.LOG)

[@@@warning "-37"]

type ('a, 'b) value = Value : 'b -> ('a, 'b) value

type ('a, 'b, 'c) thd =
  | Thd : 'b -> ('a, 'b, 'c) thd
      (** XXX(dinosaure): we must define [(_, _, _) thd] to be able to keep some
          existential types (eg. ['cfg] and ['flow] when we use
          [('cfg, 't, 'flow) service]) but still to use only on (eg. ['t]).

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
  module Endpoint : module type of Endpoint

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

  val register : ('edn, 'flow) impl -> ('edn, 'flow) protocol

  module type REPR = sig
    type t

    type flow += T of t
  end

  val repr : ('edn, 'flow) protocol -> (module REPR with type t = 'flow)

  type unpack = Flow : 'flow * (module FLOW with type flow = 'flow) -> unpack

  val unpack : flow -> unpack

  val impl :
    ('edn, 'flow) protocol ->
    (module PROTOCOL with type endpoint = 'edn and type flow = 'flow)

  val cast : flow -> ('edn, 'flow) protocol -> 'flow option

  val pack : ('edn, 'flow) protocol -> 'flow -> flow

  type 'edn resolver = Endpoint.t -> 'edn option io

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
    ?protocol:('edn, 'flow) protocol ->
    Endpoint.t ->
    (flow, [> error ]) result io

  val connect : 'edn -> ('edn, _) protocol -> (flow, [> error ]) result io

  module type SERVICE =
    Sigs.SERVICE
      with type +'a io = 'a io
       and type input = input
       and type output = output

  module Service : sig
    type ('cfg, 't, 'flow) impl =
      (module SERVICE
         with type configuration = 'cfg
          and type t = 't
          and type flow = 'flow)

    type ('cfg, 't, 'flow) t

    val equal :
      ('cfg0, 't0, 'flow0) t ->
      ('cfg1, 't1, 'flow1) t ->
      (('cfg0, 'cfg1) refl * ('t0, 't1) refl * ('flow0, 'flow1) refl) option

    val register : ('cfg, 't, 'flow) impl -> ('cfg, 't, 'flow) t

    type error = [ `Msg of string ]

    val pp_error : error Fmt.t

    val init : 'cfg -> ('cfg, 't, 'flow) t -> ('t, [> error ]) result io

    val accept : ('cfg, 't, 'flow) t -> 't -> (flow, [> error ]) result io

    val close : ('cfg, 't, 'flow) t -> 't -> (unit, [> error ]) result io

    val pack : (_, _, 'flow) t -> 'flow -> flow

    val impl :
      ('cfg, 't, 'flow) t ->
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
  module Endpoint = Endpoint
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

  type 'edn resolver = Endpoint.t -> 'edn option io

  type 'a flow_impl = (module FLOW with type flow = 'a)

  module F = struct
    type _ t = Flow : 'flow key * 'flow flow_impl -> 'flow t
  end

  module Flw = E0.Make (F)

  type flow = Flw.t = private ..

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
    let (Value (flow, Flow (_, (module Flow)))) = Flw.prj flow in
    Flow.recv flow input >>| function
    | Ok _ as v -> v
    | Error err -> Error (`Msg (strf "%a" Flow.pp_error err))

  let send flow output =
    let (Value (flow, Flow (_, (module Flow)))) = Flw.prj flow in
    Flow.send flow output >>| function
    | Ok _ as v -> v
    | Error err -> Error (`Msg (strf "%a" Flow.pp_error err))

  let close flow =
    let (Value (flow, Flow (_, (module Flow)))) = Flw.prj flow in
    Flow.close flow >>| function
    | Ok _ as v -> v
    | Error err -> Error (`Msg (strf "%a" Flow.pp_error err))

  module P = struct
    type _ t =
      | Protocol :
          'edn key * 'flow key * ('edn, 'flow) impl
          -> ('edn, 'flow) value t
  end

  module Ptr = E0.Make (P)

  type ('edn, 'flow) protocol = {
    protocol : ('edn, 'flow) value Ptr.s;
    flow : 'flow Flw.s;
  }

  let register : type edn flow. (edn, flow) impl -> (edn, flow) protocol =
   fun protocol ->
    let key s = Map.Key.create s in
    let module Flow = (val protocol) in
    let keyp = key "protocol" in
    let keyf = key "flow" in
    let flow = Flw.inj (Flow (keyf, (module Flow))) in
    let protocol = Ptr.inj (Protocol (keyp, keyf, protocol)) in
    { flow; protocol }

  module type REPR = sig
    type t

    type flow += T of t
  end

  let repr : type edn v. (edn, v) protocol -> (module REPR with type t = v) =
   fun { flow = (module Witness); _ } ->
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
   fun { protocol = (module Witness); _ } ?priority resolve ->
    let (Protocol (key, _, _)) = Witness.witness in
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
      | Ptr.Key (Protocol (kp, kf, (module Protocol))) :: r ->
      match Map.Key.(key == kp) with
      | None -> go r
      | Some E1.Refl.Refl -> (
          Protocol.connect edn >>= function
          | Ok flow ->
              let ((module Witness) : Protocol.flow Flw.s) =
                Flw.inj (Flow (kf, (module Protocol))) in
              return (Ok (Witness.T flow))
          | Error _err -> go r) in
    go (Ptr.bindings ())

  let flow_of_protocol :
      type edn flow. (edn, flow) protocol -> edn -> (flow, [> error ]) result io
      =
   fun { protocol = (module Witness); _ } edn ->
    let (Protocol (_, _, (module Protocol))) = Witness.witness in
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

  let resolve : resolvers -> Endpoint.t -> endpoint list io =
   fun m domain_name ->
    let rec go acc = function
      | [] -> return (List.rev acc) (* XXX(dinosaure): keep order. *)
      | Map.Value (k, Resolver { resolve; witness; _ }) :: r ->
      match scheduler witness with
      | None ->
          Log.warn (fun m ->
              m "A resolver with another scheduler exists in the given map.") ;
          go acc r
      | Some Refl.Refl -> (
          Log.debug (fun m -> m "Try a possible protocol.") ;
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
    Log.debug (fun m -> m "Start to resolve %a." Endpoint.pp domain_name) ;
    go [] (List.sort compare (Map.bindings m))

  let create : resolvers -> Endpoint.t -> (flow, [> error ]) result io =
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
   fun { flow = (module Witness); _ } flow -> Witness.T flow

  let resolve :
      type edn v.
      resolvers ->
      ?protocol:(edn, v) protocol ->
      Endpoint.t ->
      (flow, [> error ]) result io =
   fun m ?protocol domain_name ->
    match protocol with
    | None -> create m domain_name
    | Some ({ protocol = (module P); flow = (module F) } as f) ->
        let (Protocol (key', _, _)) = P.witness in
        resolve m domain_name >>= fun l ->
        let rec go = function
          | [] -> return (Error `Not_found)
          | Endpoint (key, edn) :: r ->
          match Map.Key.(key == key') with
          | None -> go r
          | Some E1.Refl.Refl -> (
              flow_of_protocol f edn >>= function
              | Ok flow -> return (Ok (F.T flow))
              | Error _err -> go r) in
        go l

  let connect :
      type edn v. edn -> (edn, v) protocol -> (flow, [> error ]) result io =
   fun edn { protocol = (module P); flow = (module F) } ->
    let (Protocol (_, _, (module Protocol))) = P.witness in
    Protocol.connect edn >>| reword_error (msgf "%a" Protocol.pp_error)
    >>? fun flow -> return (Ok (F.T flow))

  type unpack = Flow : 'flow * 'flow flow_impl -> unpack

  let unpack : flow -> unpack =
   fun flow ->
    let (Value (flow, Flow (_, (module Flow)))) = Flw.prj flow in
    Flow (flow, (module Flow))

  let impl :
      type edn flow.
      (edn, flow) protocol ->
      (module PROTOCOL with type endpoint = edn and type flow = flow) =
   fun { protocol = (module Witness); _ } ->
    let (Protocol (_, _, (module Protocol))) = Witness.witness in
    (module Protocol)

  let cast : type edn v. flow -> (edn, v) protocol -> v option =
   fun flow witness ->
    match Flw.extract flow witness.flow with
    | Some flow -> Some flow
    | None -> None

  module type SERVICE =
    Sigs.SERVICE
      with type +'a io = 'a io
       and type input = input
       and type output = output

  module Service = struct
    type ('cfg, 't, 'flow) impl =
      (module SERVICE
         with type configuration = 'cfg
          and type t = 't
          and type flow = 'flow)

    module S = struct
      type 't t =
        | Service : 'cfg key * ('cfg, 't, 'flow) impl -> ('cfg, 't, 'flow) thd t
    end

    module Svc = E0.Make (S)

    type ('cfg, 't, 'flow) t = {
      service : ('cfg, 't, 'flow) thd Svc.s;
      flow : 'flow Flw.s;
    }

    let register : type cfg s flow. (cfg, s, flow) impl -> (cfg, s, flow) t =
     fun service ->
      let key s = Map.Key.create s in
      let module Flow = (val service) in
      let keys = key "service" in
      let keyf = key "flow" in
      let flow = Flw.inj (Flow (keyf, (module Flow))) in
      let service = Svc.inj (Service (keys, service)) in
      { service; flow }

    type error = [ `Msg of string ]

    let pp_error ppf = function `Msg err -> Fmt.string ppf err

    let equal :
        type a b c d e f.
        (a, b, c) t ->
        (d, e, f) t ->
        ((a, d) refl * (b, e) refl * (c, f) refl) option =
     fun { service = (module A); _ } { service = (module B); _ } ->
      match A.Id with B.Id -> Some (Refl, Refl, Refl) | _ -> None

    let init :
        type cfg s flow. cfg -> (cfg, s, flow) t -> (s, [> error ]) result io =
     fun edn { service = (module Witness); _ } ->
      let (Service (_, (module Service))) = Witness.witness in
      Service.init edn >>= function
      | Ok t -> return (Ok t)
      | Error err -> return (error_msgf "%a" Service.pp_error err)

    let accept :
        type cfg s v. (cfg, s, v) t -> s -> (flow, [> error ]) result io =
     fun { service = (module Witness); flow = (module F) } t ->
      let (Service (_, (module Service))) = Witness.witness in
      Service.accept t >>= function
      | Ok flow -> return (Ok (F.T flow))
      | Error err -> return (error_msgf "%a" Service.pp_error err)

    let close :
        type cfg s flow. (cfg, s, flow) t -> s -> (unit, [> error ]) result io =
     fun { service = (module Witness); _ } t ->
      let (Service (_, (module Service))) = Witness.witness in
      Service.stop t >>= function
      | Ok () -> return (Ok ())
      | Error err -> return (error_msgf "%a" Service.pp_error err)

    let pack : type v. (_, _, v) t -> v -> flow =
     fun { flow = (module Flow); _ } flow -> Flow.T flow

    let impl :
        type cfg s flow.
        (cfg, s, flow) t ->
        (module SERVICE
           with type configuration = cfg
            and type t = s
            and type flow = flow) =
     fun { service = (module S); _ } ->
      let (Service (_, (module Service))) = S.witness in
      (module Service)
  end
end
