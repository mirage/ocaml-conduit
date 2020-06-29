type ('a, 'b) refl = Refl : ('a, 'a) refl

type resolvers
(** Type for resolvers map. *)

val empty : resolvers
(** [empty] is an empty {!resolvers} map. *)

type ('edn, 'flow) value = Value : 'flow -> ('edn, 'flow) value

module type S = sig
  type input
  (** The type for payload inputs. *)

  type output
  (** The type for payload outputs. *)

  type +'a io
  (** The type for I/O effects. *)

  type scheduler
  (** The type of I/O monads. *)

  (** {2:client Client-side conduits.} *)

  type flow = private ..
  (** The type for generic flows. {!PROTOCOL} implementations are extending (via
     {!register}) this type. It allows users to extract the underlying flow
     implementation:

     {[
       Conduit.connect domain_name >>? function
        | Conduit_lwt_unix_tcp.T Conduit.(Value (file_descr : Lwt_unix.file_descr)) -> ...
        | Conduit_lwt_unix_tls.T Conduit.(Value (fd, (tls : Tls.Engine.state))) -> ...
        | _ -> ... (* use flow functions for the default case *)
     ]}
  *)

  type error = [ `Msg of string | `Not_found ]

  val pp_error : error Fmt.t

  val recv :
    flow -> input -> ([ `Input of int | `End_of_flow ], [> error ]) result io
  (** [recv flow input] is [Ok (`Input len)] iff [n] bytes of data has been
     received from the flow [flow] and copied in [input]. *)

  val send : flow -> output -> (int, [> error ]) result io
  (** [send flow output] is [Ok n] iff [n] bytes of date from [output] has been
     sent over the flow [flow]. *)

  val close : flow -> (unit, [> error ]) result io
  (** [close flow] closes [flow]. Subsequent calls to {!recv} will return
     [Ok `End_of_flow]. Subsequent calls to {!send} will return an [Error]. *)

  (** {2:registration Protocol registration.} *)

  (** A flow is a system that allows entities to transmit {i payloads}. These
     entities do not have to care about the underlying transport mechanism.
     flows simply deal with routing and delivering of these payloads. That
     abstraction allows these protocols to compose.

      For example, the Transmission Control Protocol (TCP) is representable as a
     flow, because it is able to encapsulate some {i payloads} without
     interpreting it. A counter-example is the Simple Mail Transfer Protocol
     (SMTP) which needs an interpretation of its {i payloads}: tokens such as
     [EHLO] or [QUIT] have a direct incidence over the life-cycle of the
     connection.

      An other protocol representable as a flow is the Transport Layer Security
     (TLS), as it deals only with privacy and data integrity. [Conduit] is able
     to compose flows together like [TCP ∘ TLS] to make a new flow. Higher-level
     protocols can be built in top of these abstract flows: for instance, Secure
     Simple Mail Transfer Protocol (SSMTP) or HyperText Transfer Protocol Secure
     (HTTPS) can be defined on top of both TCP and TLS. Using [Conduit], these
     can be abstracted to work over any flow implementations. *)
  module type FLOW =
    Sigs.FLOW
      with type input = input
       and type output = output
       and type +'a io = 'a io

  (** A protocol is a {!FLOW} plus [connect]. *)
  module type PROTOCOL =
    Sigs.PROTOCOL
      with type input = input
       and type output = output
       and type +'a io = 'a io

  type ('edn, 'flow) impl =
    (module PROTOCOL with type endpoint = 'edn and type flow = 'flow)
  (** The type to represent a module {!PROTOCOL}. *)

  type ('edn, 'flow) protocol
  (** The type for client protocols. ['edn] is the type for endpoint parameters.
     ['flow] is the type for underlying flows.

      Endpoints allow users to create flows by either connecting directly to a
     remote server or by resolving domain names (with {!connect}). *)

  val register : protocol:('edn, 'flow) impl -> ('edn, 'flow) protocol
  (** [register ~protocol] is the protocol using the implementation [protocol].
     [protocol] must provide a [connect] function to allow client flows to be
     created.

      For instance, on Unix, [Conduit] clients will use [Unix.sockaddr] as flow
     endpoints, while [Unix.file_descr] would be used for the flow transport.

      {[
        module Conduit_tcp : sig
          val t : (Unix.sockaddr, Unix.file_descr) protocol
        end = struct
          let t = register ~protocol:(module TCP)
        end
      ]}

      Client endpoints can of course be more complex, for instance to hold TLS
     credentials, and [Conduit] allows all these kinds of flow to be used
     transparently:

      {[
        module Conduit_tcp_tls : sig
          val t : (Unix.sockaddr * Tls.Config.client, Unix.file_descr) protocol
        end = struct
          let t = register ~protocol:(module TLS)
        end
      ]}

      As a protocol implementer, you must {i register} your implementation and
     expose the {i witness} of it. Then, users will be able to use it. *)

  (** {2 Injection and Extraction.}

      The goal of [Conduit] is to provide:
      {ul
      {- A way to manipulate a fully-abstract [flow].}
      {- A way to manipulate a concrete and well-know [flow].}}

      [Conduit] provides several mechanisms to be able to manipulate our abstract
     type {!flow} and destruct it to a concrete value such as a [Unix.file_descr].
     [Conduit] can assert one assumption: from a given abstracted [flow], it exists
     one and only one {!FLOW} implementation.

      As [Conduit] determines this implementation, the user can determine the used
     implementation when he wants to {!send} or {!recv} datas.

      So [Conduit] uses or extracts uniqely the implementation registered before
     with {!register} and no layer can tweak or update this assertion.

      {!repr}, {!flow}, {!impl} and {!is} can extracts in differents ways the
     abstracted {!flow}:
      {ul
      {- with the {i pattern-matching}}
      {- with {i first-class module}}
      {- with the function {!is}}}
  *)

  module type REPR = sig
    type t

    type flow += T of t
  end

  val repr : ('edn, 'v) protocol -> (module REPR with type t = ('edn, 'v) value)
  (** As a protocol implementer, you should expose the concrete type of your
     flow (to be able users to {i destruct} {!flow}). [repr] returns a module
     which contains extension of {!flow} from your [protocol] such as:

      {[
        module Conduit_tcp : sig
          type t = (Unix.sockaddr, Unix.file_descr) Conduit.value
          type Conduit.flow += T of t
          val t : (Unix.sockaddr, Unix.file_descr) protocol
        end = struct
          let t = register ~protocol:(module TCP)
          include (val (Conduit.repr t))
        end
      ]}

      With this interface, users are able to {i destruct} {!flow} to your
     concrete type:

      {[
        Conduit.connect domain_name >>? function
          | Conduit_tcp.T (Conduit.Value file_descr) -> ...
          | _ -> ...
      ]}
  *)

  type unpack = Flow : 'flow * (module FLOW with type flow = 'flow) -> unpack

  val unpack : flow -> unpack
  (** [pack flow] projects the module implementation associated to the given
     abstract [flow] such as:

      {[
        Conduit.connect edn >>= fun flow ->
        let Conduit.Flow (flow, (module Flow)) = Conduit.unpack flow in
        Flow.send flow "Hello World!"
      ]}
  *)

  val impl :
    ('edn, 'flow) protocol ->
    (module PROTOCOL with type endpoint = 'edn and type flow = 'flow)
  (** [impl protocol] is [protocol]'s implementation. *)

  val cast : flow -> (_, 'flow) protocol -> 'flow option
  (** [cast flow protocol] tries to {i cast} the given [flow] to the concrete
     type described by the given [protocol].

      {[
        match Conduit.is flow Conduit_tcp.t with
        | Some (file_descr : Unix.file_descr) -> Some (Unix.getpeername file_descr)
        | None -> None
      ]}
  *)

  val pack : (_, 'v) protocol -> 'v -> flow
  (** [pack protocol concrete_flow] abstracts the given [flow] into the
     {!flow} type from a given [protocol]. It permits to use [Conduit] with a
     concrete value created by the user.

      {[
        let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        let flow = Conduit.pack Conduit_tcp.t socket in
        Conduit.send flow "Hello World!"
      ]}
  *)

  (** {2:resolution Domain name resolvers.} *)

  type 'edn resolver = [ `host ] Domain_name.t -> 'edn option io
  (** The type for resolver functions, which resolve domain names to endpoints.
     For instance, the DNS resolver function is:

     {[
       let http_resolver : Unix.sockaddr resolver =
         fun domain_name -> match Unix.gethostbyname (Domain_name.to_string domain_name) with
           | { Unix.h_addr_list; _ } ->
             if Array.length h_addr_list > 0
             then Some (Unix.ADDR_INET (h_addr_list.(0), 80))
             else None
           | exception _ -> None
     ]}
  *)

  val empty : resolvers

  val add :
    ('edn, _) protocol ->
    ?priority:int ->
    'edn resolver ->
    resolvers ->
    resolvers
  (** [add protocol ?priority resolver resolvers] adds a new resolver function
     [resolver] to [resolvers].

      When the [resolver] is able to resolve the given domain name, it will try
     to connect to the specified client endpoint. Resolvers are iterated in
     priority order (lower to higher).

      {[
        let http_resolver = ...
        let https_resolver = ... (* deal with client-side certificates here. *)

        let resolvers =
          empty
          |> add Conduit_tcp.t http_resolver
          |> add Conduit_tcp_tls.t https_resolver ~priority:10
          |> add Conduit_tcp_ssl.t https_resolver ~priority:20
      ]} *)

  val resolve :
    resolvers ->
    ?protocol:('edn, 'v) protocol ->
    [ `host ] Domain_name.t ->
    (flow, [> error ]) result io
  (** [resolve resolvers domain_name] is the flow created by connecting to the
     domain name [domain_name], using the resolvers [resolvers]. Each resolver
     tries to resolve the given domain-name (they are ordered by the given
     priority). The first which connects successfully wins.

      The resolver result is a flow connect to that winning endpoint.

      {[
        let mirage_io = domain_name_exn "mirage.io"

        val resolver_on_my_private_network : Unix.sockaddr resolver
        val resolver_on_internet : Unix.sockaddr resolver
        val resolver_with_tls : (Unix.sockaddr * Tls.Config.client) resolver

        let resolvers =
          empty
          |> add tls ~priority:0 resolver_with_tls
          |> add tcp ~priority:10 resolver_on_my_private_network
          |> add tcp ~priority:20 resolver_on_internet

        let () = Conduit.resolve resolvers mirage_io >>? function
          | TCP.T (Conduit.Value file_descr) as flow ->
            let peer = Unix.getpeername file_descr in
            ignore @@ Conduit.send flow ("Hello " ^ string_of_sockaddr peer)
          | flow ->
            ignore @@ Conduit.send flow "Hello World!"
      ]}
  *)

  val connect : 'edn -> ('edn, _) protocol -> (flow, [> error ]) result io

  (** {2:service Server-side conduits.} *)

  module type SERVICE = Sigs.SERVICE with type +'a io = 'a io

  module Service : sig
    type ('cfg, 't, 'flow) impl =
      (module SERVICE
         with type configuration = 'cfg
          and type t = 't
          and type flow = 'flow)

    type ('cfg, 't, 'flow) service
    (** The type for services, e.g. service-side protocols. ['cfg] is the type
       for configuration, ['t] is the type for state states. ['flow] is the type
       for underlying flows. *)

    val register : service:('cfg, 't, 'flow) impl -> ('cfg, 't, 'flow) service
    (** [register ~service] is the service using the implementation [service].
       [service] must define [make] and [accept] function to be able to create
       server-side flows.

         For instance:

        {[
          module TCP : SERVICE with type configuration = Unix.sockaddr
                                and type t = Unix.file_descr
                                and type flow = Unix.file_descr

          let tcp_service : (Unix.sockaddr, Unix.file_descr, Unix.file_descr) Service.service =
            Service.register ~service:(module TCP)
        ]}
    *)

    type error = [ `Msg of string ]

    val pp_error : error Fmt.t

    val init :
      'cfg -> service:('cfg, 't, 'flow) service -> ('t, [> error ]) result io
    (** [init cfg ~service] initialises the service with the
       configuration [cfg]. *)

    val accept :
      service:('cfg, 't, 'flow) service -> 't -> ('flow, [> error ]) result io
    (** [accept service t] waits for a connection on the service [t]. The result
       is a {i flow} connected to the client. *)

    val close :
      service:('cfg, 't, 'flow) service -> 't -> (unit, [> error ]) result io
    (** [close ~service t] releases the resources associated to the server [t]. *)

    val impl :
      ('cfg, 't, 'flow) service ->
      (module SERVICE
         with type configuration = 'cfg
          and type t = 't
          and type flow = 'flow)
    (** [impl service] is [service]'s underlying implementation. *)
  end
end

module type IO = Sigs.IO
(** @inline *)

module type BUFFER = Sigs.BUFFER
(** @inline *)

module Make (IO : IO) (Input : BUFFER) (Output : BUFFER) :
  S
    with type input = Input.t
     and type output = Output.t
     and type +'a io = 'a IO.t
