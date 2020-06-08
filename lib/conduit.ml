module Sigs = Sigs

type ('a, 'b) refl = Refl : ('a, 'a) refl

let strf = Format.asprintf

type _ witness = ..

type _ resolver =
  | Resolver : {
      priority : int;
      resolve : [ `host ] Domain_name.t -> ('edn option, 's) Sigs.app;
      witness : 's witness;
    }
      -> ('edn * 's) resolver

type ('a, 'b) value = Value : 'b -> ('a, 'b) value

[@@@warning "-37"]
type ('a, 'b, 'c) thd = Thd : 'b -> ('a, 'b, 'c) thd
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

  type +'a s

  type scheduler

  type flow = private ..

  type error = [ `Msg of string | `Not_found ]

  val pp_error : error Fmt.t

  val recv :
    flow -> input -> ([ `Input of int | `End_of_flow ], [> error ]) result s

  val send : flow -> output -> (int, [> error ]) result s

  val close : flow -> (unit, [> error ]) result s

  module type FLOW =
    Sigs.FLOW
      with type input = input
       and type output = output
       and type +'a s = 'a s

  module type PROTOCOL =
    Sigs.PROTOCOL
      with type input = input
       and type output = output
       and type +'a s = 'a s

  type ('edn, 'flow) impl =
    (module PROTOCOL with type endpoint = 'edn and type flow = 'flow)

  type ('edn, 'flow) protocol

  val register : protocol:('edn, 'flow) impl -> ('edn, 'flow) protocol

  module type REPR = sig
    type t

    type flow += T of t
  end

  val repr : ('edn, 'v) protocol -> (module REPR with type t = ('edn, 'v) value)

  val abstract : ('edn, 'v) protocol -> 'v -> flow

  val impl :
    ('edn, 'flow) protocol ->
    (module PROTOCOL with type endpoint = 'edn and type flow = 'flow)

  val is : flow -> ('edn, 'flow) protocol -> 'flow option

  type 'edn resolver = [ `host ] Domain_name.t -> 'edn option s

  val add :
    ('edn, 'flow) protocol ->
    ?priority:int ->
    'edn resolver ->
    resolvers ->
    resolvers

  val connect :
    resolvers ->
    ?protocol:('edn, 'v) protocol ->
    [ `host ] Domain_name.t ->
    (flow, [> error ]) result s

  module Service : sig
    module type SERVICE = Sigs.SERVICE with type +'a s = 'a s

    type ('cfg, 't, 'flow) impl =
      (module SERVICE
         with type configuration = 'cfg
          and type t = 't
          and type flow = 'flow)

    type ('cfg, 't, 'flow) service

    val register : service:('cfg, 't, 'flow) impl -> ('cfg, 't, 'flow) service

    type error = [ `Msg of string ]

    val pp_error : error Fmt.t

    val serve :
      'cfg -> service:('cfg, 't, 'flow) service -> ('t, [> error ]) result s

    val accept :
      service:('cfg, 't, 'flow) service -> 't -> ('flow, [> error ]) result s

    val close :
      service:('cfg, 't, 'flow) service -> 't -> (unit, [> error ]) result s

    val impl :
      ('cfg, 't, 'flow) service ->
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
     and type +'a s = 'a Scheduler.t = struct
  module Bijection = Sigs.Higher (Scheduler)

  type scheduler = Bijection.t

  let inj = Bijection.inj

  let prj = Bijection.prj

  let return = Scheduler.return

  let ( >>= ) x f = Scheduler.bind x f

  let ( >>| ) x f = x >>= fun x -> return (f x)

  type +'a s = 'a Scheduler.t

  type _ witness += Witness : scheduler witness

  let witness : scheduler witness = Witness

  type input = Input.t

  type output = Output.t

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

  type 'edn key = ('edn * scheduler) Map.key

  type 'edn resolver = [ `host ] Domain_name.t -> 'edn option s

  module F = struct
    type _ t =
      | Protocol : 'edn key * ('edn, 'flow) impl -> ('edn, 'flow) value t
  end

  module Ptr = E0.Make (F)

  type flow = Ptr.t = private ..

  type ('edn, 'flow) protocol = ('edn, 'flow) value Ptr.s

  let recv flow input =
    let (Ptr.Value (flow, Protocol (_, (module Protocol)))) = Ptr.prj flow in
    let (Value flow) = flow in
    Protocol.recv flow input >>| function
    | Ok _ as v -> v
    | Error err -> Error (`Msg (strf "%a" Protocol.pp_error err))

  let send flow output =
    let (Ptr.Value (flow, Protocol (_, (module Protocol)))) = Ptr.prj flow in
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

  let add :
      type edn flow.
      (edn, flow) protocol ->
      ?priority:int ->
      edn resolver ->
      resolvers ->
      resolvers =
   fun (module Witness) ?(priority = 0) resolve ->
    let (Protocol (key, _)) = Witness.witness in
    let resolve = inj <.> resolve in
    Map.add key (Resolver { priority; resolve; witness })

  type error = [ `Msg of string | `Not_found ]

  let pf ppf fmt = Format.fprintf ppf fmt

  let pp_error ppf = function
    | `Msg err -> pf ppf "%s" err
    | `Not_found -> pf ppf "Not found"

  let flow_of_endpoint : type edn. edn key -> edn -> (flow, [> error ]) result s
      =
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
      type edn flow. (edn, flow) protocol -> edn -> (flow, [> error ]) result s
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

  let resolve : resolvers -> [ `host ] Domain_name.t -> endpoint list s =
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
      (Stdlib.compare : int -> int -> int) pa pb in
    go [] (List.sort compare (Map.bindings m))

  let create :
      resolvers -> [ `host ] Domain_name.t -> (flow, [> error ]) result s =
   fun m domain_name ->
    resolve m domain_name >>= fun l ->
    let rec go = function
      | [] -> return (Error `Not_found)
      | Endpoint (key, edn) :: r -> (
          flow_of_endpoint key edn >>= function
          | Ok flow -> return (Ok flow)
          | Error _err -> go r) in
    go l

  let abstract : type edn v. (edn, v) protocol -> v -> flow =
   fun (module Witness) flow -> Witness.T (Value flow)

  let connect :
      type edn v.
      resolvers ->
      ?protocol:(edn, v) protocol ->
      [ `host ] Domain_name.t ->
      (flow, [> error ]) result s =
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

  let impl :
      type edn flow.
      (edn, flow) protocol ->
      (module PROTOCOL with type endpoint = edn and type flow = flow) =
   fun (module Witness) ->
    let (Protocol (_, (module Protocol))) = Witness.witness in
    (module Protocol)

  let is : type edn v. flow -> (edn, v) protocol -> v option =
   fun flow witness ->
    match Ptr.extract flow witness with
    | Some (Value flow) -> Some flow
    | None -> None

  module Service = struct
    module type SERVICE = Sigs.SERVICE with type +'a s = 'a s

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

    let serve :
        type cfg t flow.
        cfg -> service:(cfg, t, flow) service -> (t, [> error ]) result s =
     fun edn ~service:(module Witness) ->
      let (Service (_, (module Service))) = Witness.witness in
      Service.make edn >>= function
      | Ok t -> return (Ok t)
      | Error err -> return (error_msgf "%a" Service.pp_error err)

    let accept :
        type cfg t flow.
        service:(cfg, t, flow) service -> t -> (flow, [> error ]) result s =
     fun ~service:(module Witness) t ->
      let (Service (_, (module Service))) = Witness.witness in
      Service.accept t >>= function
      | Ok flow -> return (Ok flow)
      | Error err -> return (error_msgf "%a" Service.pp_error err)

    let close :
        type cfg t flow.
        service:(cfg, t, flow) service -> t -> (unit, [> error ]) result s =
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
