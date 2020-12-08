include Conduit_intf

let strf = Format.asprintf

(* XXX(dinosaure): the [context] must be out of the /functor/. It asserts
   that we only need [Conduit] to be able to pass a context value from a
   layer to an other.

   In results, [Conduit_lwt.context = Conduit.context]. A situation can
   appear when a layer (like an HTTP framework) don't want to choose
   between LWT, ASYNC and Mirage. But it should be able to, at least, pass
   the [context] to another layer which does this choice.
*)

module Map0 = E1.Make(struct type _ t = string end)

type _ metadata = ..
type _ metadata += Val : 'a -> 'a metadata

(* XXX(dinosaure): a [context] binds a /type witness/ to a value ([Val v]).
   It permits a kind of dynamic typing where the /type witness/ which is the
   key (in terms of [Map.Make]) gives to us the type information.

   NOTE: A trick is used here. We use a extensible variant which it will be
   extend by the application of the /functor/ below. The extension is about
   processes which can create some values from some others - such as a DNS
   resolution. However, such constructor needs to know the `IO.t` type.

   /!\ The extension should only be this particular case.
*)

module Map1 = Map0.Make(struct type 'a t = 'a metadata end)

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

[@@@warning "+37"]

let error_msgf fmt = Format.kasprintf (fun err -> Error (`Msg err)) fmt

type context = Map1.t

let empty = Map1.empty

module type S = sig
  include S with type context := context
end

module Make (IO : IO) (Input : BUFFER) (Output : BUFFER)
  : S with type input = Input.t
       and type output = Output.t
       and type +'a io = 'a IO.t
= struct
  (* Prelude *)
  let return = IO.return

  let ( >>= ) x f = IO.bind x f

  let ( >>| ) x f = IO.map f x

  let ( >>? ) x f =
    x >>= function Ok x -> f x | Error err -> return (Error err)

  type +'a io = 'a IO.t

  type input = Input.t

  type output = Output.t

  (* XXX(dinosaure): We extend values in our context by a new type:
     a function. It permits to fill the context with a resolver such
     as the DNS resolver. *)

  type 'v value = 'v Map0.key

  module Fun = struct
    type ('k, 'res) args =
      | [] : ('res, 'res) args
      | (::) : 'a arg * ('k, 'res) args -> ('a -> 'k, 'res) args
    and 'v arg =
      | Map : ('f, 'a) args * 'f -> 'a arg
      | Req : 'a value -> 'a arg
      | Opt : 'a value -> 'a option arg

    let req k = Req k
    let opt k = Opt k
    let map args k = Map (args, k)
    let ( $ ) f x = f x
  end

  type _ metadata += Fun : ('k, 'a option io) Fun.args * 'k -> 'a metadata

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

  (* XXX(dinosaure): we restrict the user to use only:
     - [recv]
     - [send]
     - [close]

     Indeed, the [connect] will be called by [Conduit] but the end-user
     should not be able to re-allocate a /flow/ ressource. The [FLOW]
     interface is interesting for CoHTTP/Git such as a simple [Mirage_flow.S]
     (with a minor update)[1] to be able to communicate with a peer.

     [1]: the minor detail is about the [recv] function which differs from
     [Mirage_flow.S.read] but the question is orthogonal to [Conduit]. *)

  module F = struct
    type 'a impl = (module FLOW with type flow = 'a)

    type _ t = Flow : 'flow impl -> 'flow t
  end

  module Flw = E0.Make (F)

  (* XXX(dinosaure): [E0] produces a /private/ and extensible variant:
     - [private] because only [Conduit] is able to extend it
       The user must use [Conduit] (and [Conduit] takes care about some details)
       to extend this variant.
     - /extensible/ to let the user to dynamically extend the knowledge of
       /protocols/. Depending on your context and specially on MirageOS, you can
       not assert that the TCP/IP protocol exists - or SSH, or TLS, etc.

       We can not assert the implementation of the protocol too. For example,
       the TCP/IP protocol can concretely exist on different forms
       ([mirage-tcpip] or the host's TCP/IP stack).

       So we let the user to /register/ its protocols at the beginning and play
       with them then. The core of [Conduit] does not assume any existence of
       any protocols.
     - [private] helps us to permit the end-user to /destruct/ the flow type to
       what it really is (a [Unix.socket], etc.). This case does not and should
       not appear for CoHTTP/Git (which want only a [FLOW]) but if layer pass the
       [flow] to the end-user (like CoHTTP), the end-user should be able to
       introspect the value and /destruct/ it to the underlying /socket/. *)

  type flow = Flw.t = private ..

  module type REPR = sig
    type t

    type flow += T of t
  end

  (* XXX(dinosaure): [REPR] is the way to re-export the /constructor/. It
     permits to the protocol implementer to let the user to destruct the
     type [flow] to its own new type. *)

  type 'a repr = (module REPR with type t = 'a)

  module Flow = struct
    type 'a impl = 'a F.impl

    type 'a t = 'a Flw.s

    let register : type flow. flow impl -> flow t =
     fun flow ->
      Flw.inj (Flow flow)

    let repr : type flow. flow t -> (module REPR with type t = flow) =
     fun (module Witness) ->
      let module M = struct
        include Witness

        type t = x
      end in
      (module M)
  end

  (* XXX(dinosaure): note about performance, [Ptr.prj] can cost where
     it's a lookup into the global [hashtbl] (created by [Ptr]). However,
     the usual pattern of [Conduit] is multiple calls of [send]/[recv] with
     the same [flow].

     Implementation of internal [hashtbl] memoize such case. We have different
     overheads:
     - about [recv]/[send], it's around ~500ns (first call), ~125ns (subsequent calls)
     - about [flow] & [Flow.recv]/[Flow.send], it's aroung ~75ns

     However, keep in your mind that:
     - the internal [hashtbl] should be small (smaller than 16 elements)
     - performance is intrinsic with [caml_hash]

     NOTE: about performance, if we are really aware about that, the best to use
     [Conduit] and the underlying protocol is to /project/ the implementation
     with [Conduit.impl] and us it as is - by this way, it's like a use of a
     first-class module.
   *)

  let recv flow =
    let (Value (flow, Flow (module Protocol))) = Flw.prj flow in
    fun input ->
      Protocol.recv flow input >>| function
      | Ok _ as v -> v
      | Error err -> Error (`Msg (strf "%a" Protocol.pp_error err))

  let send flow =
    let (Value (flow, Flow (module Protocol))) = Flw.prj flow in
    fun output ->
      Protocol.send flow output >>| function
      | Ok _ as v -> v
      | Error err -> Error (`Msg (strf "%a" Protocol.pp_error err))

  let close flow =
    let (Value (flow, Flow (module Protocol))) = Flw.prj flow in
    Protocol.close flow >>| function
    | Ok _ as v -> v
    | Error err -> Error (`Msg (strf "%a" Protocol.pp_error err))

  let empty = empty

  let info ~name = Map0.Key.create name

  let add k v m = Map1.add k (Val v) m

  let rec apply
    : type k res. context -> (k, res option io) Fun.args -> k -> res option io
    = fun ctx args f ->
      let rec go : type k res. context -> (k, res) Fun.args -> k -> res io = fun ctx -> function
        | [] -> fun x -> return x
        | Map (args', f') :: tl ->
          fun f -> go ctx args' f' >>= fun v -> go ctx tl (f v)
        | Opt key :: tl -> fun f -> find key ctx >>= fun v -> go ctx tl (f v)
        | Req key :: tl ->
          fun f -> find key ctx >>= function
            | Some v -> go ctx tl (f v)
            | None -> raise Not_found in
      try go ctx args f >>= fun fiber -> fiber >>= fun v -> return v
      with Not_found -> return None

  and find
    : type a. a value -> context -> a option io
    = fun info ctx ->
      match Map1.find info ctx with
      | None -> return None
      | Some (Val v) -> return (Some v)
      | Some (Fun (args, f)) -> apply ctx args f
      | _ -> return None

  let fold
    : type k res. res value -> (k, res option io) Fun.args -> f:k -> context -> context
    = fun key args ~f ctx ->
      Map1.add key (Fun (args, f)) ctx

  module P = struct
    [@@@warning "-37"]

    type ('edn, 'flow) snd = Snd : 'flow -> ('edn, 'flow) snd
    (* XXX(dinosaure): this is the same trick as [thd] to keep
       existentials. *)

    [@@@warning "+37"]

    type _ t =
      | Protocol :
          'edn Map0.key * 'flow Flow.t * ('edn, 'flow) impl
          -> ('edn, 'flow) snd t
  end

  module Ptr = E0.Make (P)
  (* XXX(dinosaure): extension of [Flow] with [connect] - such map
     is used only by [Conduit]. *)

  type ('edn, 'flow) protocol = {
    protocol : ('edn, 'flow) P.snd Ptr.s;
    flow : 'flow Flow.t;
  }

  let register : type edn flow. name:string -> (edn, flow) impl -> edn value * (edn, flow) protocol =
   fun ~name (module M) ->
    let witness = Map0.Key.create name in
    let flow = Flow.register (module M) in
    let protocol = Ptr.inj (Protocol (witness, flow, (module M))) in
    witness, { flow; protocol }

  let repr t = Flow.repr t.flow

  type error = [ `Msg of string | `Not_found ]

  let pf ppf fmt = Format.fprintf ppf fmt

  let pp_error ppf = function
    | `Msg err -> pf ppf "%s" err
    | `Not_found -> pf ppf "Not found"

  let flow_of_value
    : type edn. edn value -> edn -> (flow, [> error ]) result io
    = fun key v ->
    let rec go : Ptr.k list -> _ = function
      | [] -> return (Error `Not_found)
      | Ptr.Key (Protocol (key', (module Flow), (module Protocol))) :: r ->
        ( match Map0.Key.(key == key') with
        | None -> go r
        | Some E1.Refl.Refl -> (
            Protocol.connect v >>= function
            | Ok flow -> return (Ok (Flow.T flow))
            | Error _err -> go r) ) in
    go (Ptr.bindings ())

  let flow_of_protocol :
      type edn flow. (edn, flow) protocol -> edn -> (flow, [> error ]) result io
      =
   fun { protocol = (module Witness); _ } edn ->
    let (Protocol (_, _, (module Protocol))) = Witness.witness in
    Protocol.connect edn >>= function
    | Ok flow -> return (Ok flow)
    | Error err -> return (error_msgf "%a" Protocol.pp_error err)

  type packed = Value : 'v value * 'v -> packed

  let resolve : context -> packed list io =
   fun m ->
    let rec go acc : Map1.v list -> _ = function
      | [] -> return (List.rev acc)
      | Map1.Value (k, Val v) :: r -> go (Value (k, v) :: acc) r
      | Map1.Value (k, Fun (args, f)) :: r ->
        ( apply m args f >>= function
        | Some v -> go (Value (k, v) :: acc) r
        | None -> go acc r )
      | _ :: r -> go acc r in
    go [] (Map1.bindings m)

  let create : context -> (flow, [> error ]) result io =
   fun m ->
    resolve m >>= fun l ->
    let rec go : packed list -> _ = function
      | [] -> return (Error `Not_found)
      | Value (key, v) :: r -> (
          flow_of_value key v >>= function
          | Ok flow -> return (Ok flow)
          | Error _err -> go r) in
    go l

  let resolve :
      type edn v.
      ?protocol:(edn, v) protocol ->
      context ->
      (flow, [> error ]) result io =
   fun ?protocol m ->
    match protocol with
    | None -> create m
    | Some protocol ->
        let (module Protocol) = protocol.protocol in
        let (module Flow) = protocol.flow in
        let Protocol (key', _, _) = Protocol.witness in
        resolve m >>= fun l ->
        let rec go : packed list -> _ = function
          | [] -> return (Error `Not_found)
          | Value (key, edn) :: r -> match Map0.Key.(key == key') with
            | None -> go r
            | Some E1.Refl.Refl -> (
                flow_of_protocol protocol edn >>= function
                | Ok flow -> return (Ok (Flow.T flow))
                | Error _err -> go r) in
        go l

  let connect :
      type edn v. (edn, v) protocol -> edn -> (flow, [> error ]) result io =
   fun { protocol = (module Witness); _ } ->
    let (Protocol (_, (module Flow), (module Protocol))) = Witness.witness in
    fun edn ->
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
        | Svc : 'cfg Map0.key * ('cfg, 't, 'flow) impl -> ('cfg, 't, 'flow) thd t
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
      let cfg = Map0.Key.create "" in
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
     fun (Service ((module Witness), _)) ->
      let (Svc (_, (module Service))) = Witness.witness in
      fun cfg ->
        Service.init cfg >>= function
        | Ok t -> return (Ok t)
        | Error err -> return (error_msgf "%a" Service.pp_error err)

    let accept :
        type cfg s v. (cfg, s, v) t -> s -> (flow, [> error ]) result io =
     fun (Service ((module Witness), { flow = (module Flow); _ })) ->
      let (Svc (_, (module Service))) = Witness.witness in
      fun t ->
        Service.accept t >>= function
        | Ok flow -> return (Ok (Flow.T flow))
        | Error err -> return (error_msgf "%a" Service.pp_error err)

    let stop :
        type cfg s flow. (cfg, s, flow) t -> s -> (unit, [> error ]) result io =
     fun (Service ((module Witness), _)) ->
      let (Svc (_, (module Service))) = Witness.witness in
      fun t ->
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
