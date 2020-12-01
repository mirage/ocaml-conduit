module Endpoint = Endpoint
include Conduit_intf

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
    PROTOCOL
      with type input = input
       and type output = output
       and type +'a io = 'a io

  module type FLOW =
    FLOW
      with type input = input
       and type output = output
       and type +'a io = 'a io

  type ('edn, 'flow) impl =
    (module PROTOCOL with type endpoint = 'edn and type flow = 'flow)

  type 'edn key = ('edn * scheduler) Map.key

  type 'edn resolver = Endpoint.t -> 'edn option io

  module F = struct
    type 'a impl = (module FLOW with type flow = 'a)

    type _ t = Flow : 'flow key * 'flow impl -> 'flow t
  end

  module Flw = E0.Make (F)

  type flow = Flw.t = private ..

  module type REPR = sig
    type t

    type flow += T of t
  end

  type 'a repr = (module REPR with type t = 'a)

  module Flow = struct
    type 'a impl = 'a F.impl

    type 'a t = 'a Flw.s

    let register : type flow. flow impl -> flow t =
     fun flow ->
      let key = Map.Key.create "" in
      Flw.inj (Flow (key, flow))

    let repr : type flow. flow t -> (module REPR with type t = flow) =
     fun (module Witness) ->
      let module M = struct
        include Witness

        type t = x
      end in
      (module M)
  end

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
    let (Value (flow, Flow (_, (module Protocol)))) = Flw.prj flow in
    Protocol.recv flow input >>| function
    | Ok _ as v -> v
    | Error err -> Error (`Msg (strf "%a" Protocol.pp_error err))

  let send flow output =
    let (Value (flow, Flow (_, (module Protocol)))) = Flw.prj flow in
    Protocol.send flow output >>| function
    | Ok _ as v -> v
    | Error err -> Error (`Msg (strf "%a" Protocol.pp_error err))

  let close flow =
    let (Value (flow, Flow (_, (module Protocol)))) = Flw.prj flow in
    Protocol.close flow >>| function
    | Ok _ as v -> v
    | Error err -> Error (`Msg (strf "%a" Protocol.pp_error err))

  module P = struct
    type ('edn, 'flow) value = Value : 'flow -> ('edn, 'flow) value

    type _ t =
      | Protocol :
          'edn key * 'flow Flow.t * ('edn, 'flow) impl
          -> ('edn, 'flow) value t
  end

  module Ptr = E0.Make (P)

  type ('edn, 'flow) protocol = {
    protocol : ('edn, 'flow) P.value Ptr.s;
    flow : 'flow Flow.t;
  }

  let register : type edn flow. (edn, flow) impl -> (edn, flow) protocol =
   fun (module M) ->
    let flow = Flow.register (module M) in
    let key = Map.Key.create "" in
    let protocol = Ptr.inj (Protocol (key, flow, (module M))) in
    { flow; protocol }

  let repr t = Flow.repr t.flow

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

  type error = [ `Msg of string | `Not_found of Endpoint.t ]

  let pf ppf fmt = Format.fprintf ppf fmt

  let pp_error ppf = function
    | `Msg err -> pf ppf "%s" err
    | `Not_found edn -> pf ppf "%a not found" Endpoint.pp edn

  let flow_of_endpoint :
      type edn. edn:Endpoint.t -> edn key -> edn -> (flow, [> error ]) result io
      =
   fun ~edn key v ->
    let rec go = function
      | [] -> return (Error (`Not_found edn))
      | Ptr.Key (Protocol (k, (module Flow), (module Protocol))) :: r ->
      match Map.Key.(key == k) with
      | None -> go r
      | Some E1.Refl.Refl -> (
          Protocol.connect v >>= function
          | Ok flow -> return (Ok (Flow.T flow))
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
   fun m edn ->
    resolve m edn >>= fun l ->
    let rec go = function
      | [] -> return (Error (`Not_found edn))
      | Endpoint (key, v) :: r -> (
          flow_of_endpoint ~edn key v >>= function
          | Ok flow -> return (Ok flow)
          | Error _err -> go r) in
    go l

  let resolve :
      type edn v.
      resolvers ->
      ?protocol:(edn, v) protocol ->
      Endpoint.t ->
      (flow, [> error ]) result io =
   fun m ?protocol edn ->
    match protocol with
    | None -> create m edn
    | Some protocol ->
        let (module Protocol) = protocol.protocol in
        let (module Flow) = protocol.flow in
        let (Protocol (key', _, _)) = Protocol.witness in
        resolve m edn >>= fun l ->
        let rec go = function
          | [] -> return (Error (`Not_found edn))
          | Endpoint (key, edn) :: r ->
          match Map.Key.(key == key') with
          | None -> go r
          | Some E1.Refl.Refl -> (
              flow_of_protocol protocol edn >>= function
              | Ok flow -> return (Ok (Flow.T flow))
              | Error _err -> go r) in
        go l

  let connect :
      type edn v. (edn, v) protocol -> edn -> (flow, [> error ]) result io =
   fun { protocol = (module Witness); _ } edn ->
    let (Protocol (_, (module Flow), (module Protocol))) = Witness.witness in
    Protocol.connect edn >>| reword_error (msgf "%a" Protocol.pp_error)
    >>? fun flow -> return (Ok (Flow.T flow))

  let impl :
      type edn flow.
      (edn, flow) protocol ->
      (module PROTOCOL with type endpoint = edn and type flow = flow) =
   fun { protocol = (module Witness); _ } ->
    let (Protocol (_, _, (module Protocol))) = Witness.witness in
    (module Protocol)

  let cast : type edn v. flow -> (edn, v) protocol -> v option =
   fun flow witness -> Flw.extract flow witness.flow

  module type SERVICE = SERVICE with type +'a io = 'a io

  module Service = struct
    type ('cfg, 't, 'flow) impl =
      (module SERVICE
         with type configuration = 'cfg
          and type t = 't
          and type flow = 'flow)

    module F = struct
      type 't t =
        | Svc : 'cfg key * ('cfg, 't, 'flow) impl -> ('cfg, 't, 'flow) thd t
    end

    module Svc = E0.Make (F)

    type ('cfg, 't, 'flow) t =
      | Service :
          ('cfg, 't, 'flow) thd Svc.s * (_, 'flow) protocol
          -> ('cfg, 't, 'flow) t

    let register :
        type cfg s flow.
        (cfg, s, flow) impl -> (_, flow) protocol -> (cfg, s, flow) t =
     fun service protocol ->
      let cfg = Map.Key.create "" in
      Service (Svc.inj (Svc (cfg, service)), protocol)

    type error = [ `Msg of string ]

    let pp_error ppf = function `Msg err -> Fmt.string ppf err

    let equal :
        type a b c d e f.
        (a, b, c) t ->
        (d, e, f) t ->
        ((a, d) refl * (b, e) refl * (c, f) refl) option =
     fun (Service ((module A), _)) (Service ((module B), _)) ->
      match A.Id with B.Id -> Some (Refl, Refl, Refl) | _ -> None

    let init :
        type cfg s flow. (cfg, s, flow) t -> cfg -> (s, [> error ]) result io =
     fun (Service ((module Witness), _)) cfg ->
      let (Svc (_, (module Service))) = Witness.witness in
      Service.init cfg >>= function
      | Ok t -> return (Ok t)
      | Error err -> return (error_msgf "%a" Service.pp_error err)

    let accept :
        type cfg s v. (cfg, s, v) t -> s -> (flow, [> error ]) result io =
     fun (Service ((module Witness), { flow = (module Flow); _ })) t ->
      let (Svc (_, (module Service))) = Witness.witness in
      Service.accept t >>= function
      | Ok flow -> return (Ok (Flow.T flow))
      | Error err -> return (error_msgf "%a" Service.pp_error err)

    let stop :
        type cfg s flow. (cfg, s, flow) t -> s -> (unit, [> error ]) result io =
     fun (Service ((module Witness), _)) t ->
      let (Svc (_, (module Service))) = Witness.witness in
      Service.stop t >>= function
      | Ok () -> return (Ok ())
      | Error err -> return (error_msgf "%a" Service.pp_error err)

    let flow : type v. (_, _, v) t -> v -> flow =
     fun (Service (_, { flow = (module Witness); _ })) flow -> Witness.T flow

    let impl :
        type cfg s flow.
        (cfg, s, flow) t ->
        (module SERVICE
           with type configuration = cfg
            and type t = s
            and type flow = flow) =
     fun (Service ((module S), _)) ->
      let (Svc (_, (module Service))) = S.witness in
      (module Service)
  end
end
