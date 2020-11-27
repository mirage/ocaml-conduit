module Endpoint = Endpoint
module Sigs = Sigs

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

module type S = Conduit_impl.S

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

  module type REPR = sig
    type t

    type flow += T of t
  end

  type 'a repr = (module REPR with type t = 'a)

  module Flow = struct
    type 'a impl = (module FLOW with type flow = 'a)

    type 'a t = 'a Flw.s

    let register : type flow. flow impl -> flow t =
     fun flow ->
      let key = Map.Key.create "" in
      Flw.inj (Flow (key, flow))

    let impl : type flow. flow t -> flow impl =
     fun (module Flow) ->
      let (Flow (_, m)) = Flow.witness in
      m

    let repr : type flow. flow t -> (module REPR with type t = flow) =
     fun (module Witness) ->
      let module M = struct
        include Witness

        type t = x
      end in
      (module M)
  end

  type 'a t = 'a Flow.t

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
          'edn key * 'flow Flow.t * ('edn, 'flow) impl
          -> ('edn, 'flow) value t

    let impl (Protocol (_, _, m)) = m
  end

  module Ptr = E0.Make (P)

  type ('edn, 'flow) protocol = {
    protocol : ('edn, 'flow) value Ptr.s;
    flow : 'flow Flw.s;
  }

  let register :
      type edn flow. flow t -> (edn, flow) impl -> (edn, flow) protocol =
   fun flow protocol ->
    let key = Map.Key.create "" in
    let protocol = Ptr.inj (Protocol (key, flow, protocol)) in
    { flow; protocol }

  let impl : type edn flow. (edn, flow) protocol -> (edn, flow) impl =
   fun { protocol = (module Witness); _ } -> P.impl Witness.witness

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

  let flow_of_endpoint : type edn. edn key -> edn -> (flow, error) result io =
   fun key edn ->
    let rec go = function
      | [] -> return (Error `Not_found)
      | Ptr.Key (Protocol (k, (module Flow), (module Protocol))) :: r ->
      match Map.Key.(key == k) with
      | None -> go r
      | Some E1.Refl.Refl -> (
          Protocol.connect edn >>= function
          | Ok flow -> return (Ok (Flow.T flow))
          | Error _err -> go r) in
    go (Ptr.bindings ())

  type endpoint = Endpoint : 'edn key * 'edn -> endpoint

  module Refl = struct
    type ('a, 'b) t = Refl : ('a, 'a) t
  end

  let scheduler : type s. s witness -> (s, scheduler) Refl.t option = function
    | Witness -> Some Refl.Refl
    | _ -> None

  let inf = -1

  and sup = 1

  let resolve_to_endpoints : resolvers -> Endpoint.t -> endpoint list io =
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

  let resolve : resolvers -> Endpoint.t -> (flow, error) result io =
   fun m domain_name ->
    resolve_to_endpoints m domain_name >>= fun l ->
    let rec go = function
      | [] -> return (Error `Not_found)
      | Endpoint (key, edn) :: r -> (
          flow_of_endpoint key edn >>= function
          | Ok flow -> return (Ok flow)
          | Error _err -> go r) in
    go l

  let connect : type edn v. (edn, v) protocol -> edn -> (flow, error) result io
      =
   fun { protocol = (module P); flow = (module F) } edn ->
    let (Protocol (_, _, (module Protocol))) = P.witness in
    Protocol.connect edn >>= function
    | Ok flow -> return (Ok (F.T flow))
    | Error e ->
        let err = Fmt.str "%a" Protocol.pp_error e in
        return (Error (`Msg err))

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
        | Service :
            'cfg key * 'flow Flow.t * ('cfg, 't, 'flow) impl
            -> ('cfg, 't, 'flow) thd t

      let impl (Service (_, _, i)) = i
    end

    module Svc = E0.Make (S)

    type ('cfg, 't, 'flow) t = {
      service : ('cfg, 't, 'flow) thd Svc.s;
      flow : 'flow Flw.s;
    }

    let register :
        type cfg s flow. flow Flow.t -> (cfg, s, flow) impl -> (cfg, s, flow) t
        =
     fun flow service ->
      let key = Map.Key.create "" in
      let service = Svc.inj (Service (key, flow, service)) in
      { service; flow }

    let impl : type cfg s flow. (cfg, s, flow) t -> (cfg, s, flow) impl =
     fun { service = (module Witness); _ } -> S.impl Witness.witness

    type error = [ `Msg of string ]

    let pp_error ppf = function `Msg err -> Fmt.string ppf err

    let repr t = Flow.repr t.flow

    let init : type cfg s flow. (cfg, s, flow) t -> cfg -> (s, error) result io
        =
     fun { service = (module Witness); _ } edn ->
      let (module Service) = S.impl Witness.witness in
      Service.init edn >>= function
      | Ok t -> return (Ok t)
      | Error err -> return (error_msgf "%a" Service.pp_error err)

    let accept : type cfg s v. (cfg, s, v) t -> s -> (flow, error) result io =
     fun { service = (module Witness); flow = (module F) } t ->
      let (module Service) = S.impl Witness.witness in
      Service.accept t >>= function
      | Ok flow -> return (Ok (F.T flow))
      | Error err -> return (error_msgf "%a" Service.pp_error err)

    let stop : type cfg s flow. (cfg, s, flow) t -> s -> (unit, error) result io
        =
     fun { service = (module Witness); _ } t ->
      let (module Service) = S.impl Witness.witness in
      Service.stop t >>= function
      | Ok () -> return (Ok ())
      | Error err -> return (error_msgf "%a" Service.pp_error err)
  end
end
