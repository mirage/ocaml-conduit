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
  type +'a io

  type flow

  type t

  type error

  type configuration

  val init : configuration -> (t, error) result io

  val pp_error : error Fmt.t

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

type ('a, 'b) refl = Refl : ('a, 'a) refl

module type S = sig
  module Endpoint : module type of Endpoint

  type input
  (** The type for payload inputs. *)

  type output
  (** The type for payload outputs. *)

  type +'a io
  (** The type for I/O effects. *)

  (** {2:client Client-side conduits.} *)

  type flow = private ..
  (** The type for generic flows. {!PROTOCOL} implementations are extending (via
      {!register}) this type. It allows users to extract the underlying flow
      implementation:

      {[
        connect domain_name >>? function
         | Conduit_lwt_unix_tcp.T (file_descr : Lwt_unix.file_descr) -> ...
         | Conduit_lwt_unix_tls.T (_, (tls : Tls.Engine.state)) -> ...
         | _ -> ... (* use flow functions for the default case *)
      ]} *)

  type error = [ `Msg of string | `Not_found of Endpoint.t ]

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
      to compose flows together like [TCP ∘ TLS] to make a new flow.
      Higher-level protocols can be built in top of these abstract flows: for
      instance, Secure Simple Mail Transfer Protocol (SSMTP) or HyperText
      Transfer Protocol Secure (HTTPS) can be defined on top of both TCP and
      TLS. Using [Conduit], these can be abstracted to work over any flow
      implementations. *)
  module type FLOW =
    FLOW
      with type input = input
       and type output = output
       and type +'a io = 'a io

  (** A protocol is a {!FLOW} plus [connect]. *)
  module type PROTOCOL =
    PROTOCOL
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

  val register : ('edn, 'flow) impl -> ('edn, 'flow) protocol
  (** [register i] is the protocol using the implementation [i]. [protocol] must
      provide a [connect] function to allow client flows to be created.

      For instance, on Unix, [Conduit] clients will use [Unix.sockaddr] as flow
      endpoints, while [Unix.file_descr] would be used for the flow transport.

      {[
        module TCP :
          PROTOCOL
            with type endpoint = Unix.sockaddr
             and type flow = Unix.file_descr = struct ... end

        module Conduit_tcp : sig
          val t : (Unix.sockaddr, Unix.file_descr) protocol
        end = struct
          let t = register (module TCP)
        end
      ]}

      Client endpoints can of course be more complex, for instance to hold TLS
      credentials, and [Conduit] allows all these kinds of flow to be used
      transparently:

      {[
        module TLS :
          PROTOCOL
            with type endpoint = Unix.sockaddr * Tls.config_client
             and type flow = Unix.file_descr = struct ... end

        module Conduit_tcp_tls : sig
          val t : (Unix.sockaddr * Tls.Config.client, Unix.file_descr) protocol
        end = struct
          let t = register (module TLS)
        end
      ]}

      As a protocol implementer, you must {i register} your implementation and
      expose the {i witness} of it. Then, users will be able to use it. *)

  (** {2 Injection and Extraction.}

      The goal of [Conduit] is to provide:

      - A way to manipulate a fully-abstract [flow].
      - A way to manipulate a concrete and well-know [flow].

      [Conduit] provides several mechanisms to be able to manipulate our
      abstract type {!flow} and destruct it to a concrete value such as a
      [Unix.file_descr]. [Conduit] can assert one assumption: from a given
      abstracted [flow], it exists one and only one {!FLOW} implementation.

      As [Conduit] determines this implementation, the user can determine the
      used implementation when he wants to {!send} or {!recv} datas.

      So [Conduit] uses or extracts uniqely the implementation registered before
      with {!register} and no layer can tweak or update this assertion.

      {!repr}, {!flow}, {!impl} and {!is} can extracts in differents ways the
      abstracted {!flow}:

      - with the {i pattern-matching}
      - with {i first-class module}
      - with the function {!is} *)

  module type REPR = sig
    type t

    type flow += T of t
  end

  type 'a repr = (module REPR with type t = 'a)
  (** The type for {!REPR} values. *)

  val repr : (_, 'flow) protocol -> 'flow repr
  (** [repr t] is a module which contains the concrete representation of flow
      values. It can then be used to destruct {!flow} values, via
      pattern-matching. For instance, For to set the underlying file-decriptor
      as non-blocking, one can do:

      {[
        module TCP :
          PROTOCOL
            with type endpoint = Unix.sockaddr
             and type flow = Unix.file_descr = struct ... end

        module Conduit_tcp : sig
          type flow += T of Unix.file_descr

          val t : (Unix.sockaddr, Unix.file_descr) protocol
        end = struct
          let t = register (module TCP)

          include (val repr t)
        end

        let set_nonblock (flow : flow) =
          match flow with Conduit_tcp.T fd -> Unix.set_nonblock fd | _ -> ()
      ]} *)

  val impl : ('edn, 'flow) protocol -> ('edn, 'flow) impl
  (** [impl protocol] is [protocol]'s implementation. *)

  val cast : flow -> (_, 'flow) protocol -> 'flow option
  (** [cast flow protocol] tries to {i cast} the given [flow] to the concrete
      type described by the given [protocol].

      {[
        match cast flow Conduit_tcp.t with
        | Some (fd : Unix.file_descr) -> Some (Unix.getpeername fd)
        | None -> None
      ]} *)

  (** {2:resolution Domain name resolvers.} *)

  type 'edn resolver = Endpoint.t -> 'edn option io
  (** The type for resolver functions, which resolve domain names to endpoints.
      For instance, the DNS resolver function is:

      {[
        let http_resolver : Unix.sockaddr resolver = function
          | IP ip -> Some (Ipaddr_unix.to_inet_addr ip, 80)
          | Domain domain_name ->
          match Unix.gethostbyname (Domain_name.to_string domain_name) with
          | { Unix.h_addr_list; _ } ->
              if Array.length h_addr_list > 0
              then Some (Unix.ADDR_INET (h_addr_list.(0), 80))
              else None
          | exception _ -> None
      ]} *)

  type resolvers

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
    Endpoint.t ->
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

        let () =
          resolve resolvers (Endpoint.domain mirage_io) >>? function
          | TCP.T file_descr as flow ->
              let peer = Unix.getpeername file_descr in
              ignore @@ send flow ("Hello " ^ string_of_sockaddr peer)
          | flow -> ignore @@ send flow "Hello World!"
      ]} *)

  val connect : ('edn, _) protocol -> 'edn -> (flow, [> error ]) result io

  (** {2:service Server-side conduits.} *)

  module type SERVICE = SERVICE with type +'a io = 'a io

  module Service : sig
    type ('cfg, 't, 'flow) impl =
      (module SERVICE
         with type configuration = 'cfg
          and type t = 't
          and type flow = 'flow)

    type ('cfg, 't, 'flow) t
    (** The type for services, e.g. service-side protocols. ['cfg] is the type
        for configuration, ['s] is the type for server states. ['flow] is the
        type for underlying flows. *)

    val equal :
      ('cfg0, 't0, 'flow0) t ->
      ('cfg1, 't1, 'flow1) t ->
      (('cfg0, 'cfg1) refl * ('t0, 't1) refl * ('flow0, 'flow1) refl) option
    (** [equal svc0 svc1] proves that [svc0] and [svc1] are physically the same.
        For instance, [Conduit] asserts:

        {[
          let service = Service.register (module V) protocol ;;

          let () = match Service.equal service service with
            | Some (Refl, Refl, Refl) -> ...
            | _ -> assert false
        ]} *)

    val register : ('cfg, 't, 'v) impl -> (_, 'v) protocol -> ('cfg, 't, 'v) t
    (** [register i p] is the service using the implementation [i] using the
        protocol [p]. [i] should define a [make] and an [accept] function to
        create server-side flows.

        For instance:

        {[
          module TCP_service :
            SERVICE
              with type configuration = Unix.sockaddr
               and type t = Unix.file_descr
               and type flow = Unix.file_descr = struct ... end

          let tcp_protocol = register (module TCP_protocol)

          let tcp_service :
              (Unix.sockaddr, Unix.file_descr, Unix.file_descr) Service.t
              =
            Service.register (module TCP_service) tcp_protocol
        ]} *)

    type error = [ `Msg of string ]

    val pp_error : error Fmt.t

    val init : ('cfg, 't, 'v) t -> 'cfg -> ('t, [> error ]) result io
    (** [init t cfg] initialises the service with the configuration [cfg]. *)

    val accept : ('cfg, 's, 'v) t -> 's -> (flow, [> error ]) result io
    (** [accept t s] waits for a connection on the server [s]. The result is a
        {i flow} connected to the client. *)

    val stop : ('cfg, 's, 'v) t -> 's -> (unit, [> error ]) result io
    (** [stop t s] releases the resources associated to the server [s]. *)

    val flow : (_, _, 'v) t -> 'v -> flow
    (** [flow t s] is the [s] seen as a an abstract {!flow}.

        {[
          let handler (flow : flow) =
            send flow "Hello World!" >>= fun _ ->
            ...

          let run service cfg =
            let module S = Service.impl service in
            Service.init cfg >>? fun t ->
            let rec loop t =
              S.accept t >>? fun flow ->
              let flow = Service.flow service flow in
              async (fun () -> handler flow) ; loop t in
            loop t

          let () = run tcp_service (localhost, 8080)
          let () = run tls_service (certs, (localhost, 8080))
        ]} *)

    val impl :
      ('cfg, 't, 'v) t ->
      (module SERVICE
         with type configuration = 'cfg
          and type t = 't
          and type flow = 'v)
    (** [impl service] is [service]'s underlying implementation. *)
  end
end
