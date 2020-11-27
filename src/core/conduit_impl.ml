module type S = sig
  type input
  (** The type for payload inputs. *)

  type output
  (** The type for payload outputs. *)

  type +'a io
  (** The type for I/O effects. *)

  (** {2:flow Abstract flows.} *)

  (** A flow is an abstract concept that allows users to read and transmit
      {i payloads}, without caring about the underlying transport mechanism.
      Flows simply deal with routing and delivering of these payloads. That
      abstraction allows these protocols to compose.

      For example, the Transmission Control Protocol (TCP) is representable as a
      flow, because it is able to encapsulate some {i payloads} without
      interpreting it. An other protocol representable as a flow is the
      Transport Layer Security (TLS), as it deals only with privacy and data
      integrity. A counter-example is the Simple Mail Transfer Protocol (SMTP)
      which needs an interpretation of its {i payloads}: tokens such as [EHLO]
      or [QUIT] have a direct incidence over the life-cycle of the connection.

      [Conduit] is able to compose flows together like [TCP ∘ TLS] to make a
      new flow. Higher-level protocols can be built in top of these abstract
      flows: for instance, Secure Simple Mail Transfer Protocol (SSMTP) or
      HyperText Transfer Protocol Secure (HTTPS) can be defined on top of both
      TCP and TLS. Using [Conduit], these can be abstracted to work over any
      flow implementations. *)

  type flow = private ..
  (** The type for generic flows. Flows implementations can extend this type via
      the {!Flow.register}) function. Once extended, the underlying flow value
      is still available:

      {[
        Conduit.connect domain_name >>? function
         | Conduit_lwt_unix_tcp.T (fd : Lwt_unix.file_descr) -> ...
         | Conduit_lwt_unix_ssl.T (fd : Lwt_ssl.socket) -> ...
         | _ -> ... (* use flow functions for the default case *)
      ]} *)

  type error = private [> `Msg of string ]
  (** The type for flow errors. *)

  val pp_error : error Fmt.t

  val recv :
    flow -> input -> ([ `Input of int | `End_of_flow ], error) result io
  (** [recv flow input] is [Ok (`Input len)] iff [n] bytes of data has been
      received from the flow [flow] and copied in [input]. *)

  val send : flow -> output -> (int, error) result io
  (** [send flow output] is [Ok n] iff [n] bytes of date from [output] has been
      sent over the flow [flow]. *)

  val close : flow -> (unit, error) result io
  (** [close flow] closes [flow]. Subsequent calls to {!recv} will return
      [Ok `End_of_flow]. Subsequent calls to {!send} will return an [Error]. *)

  module type FLOW =
    Sigs.FLOW
      with type input = input
       and type output = output
       and type +'a io = 'a io

  (** {2:connect Connections.} *)

  (** A protocol is a {!FLOW} with a [connect] function. *)
  module type PROTOCOL =
    Sigs.PROTOCOL
      with type input = input
       and type output = output
       and type +'a io = 'a io

  type ('edn, 'flow) impl =
    (module PROTOCOL with type endpoint = 'edn and type flow = 'flow)
  (** The type to represent {!PROTOCOL} module implementations. *)

  type ('edn, 'flow) protocol
  (** The type for client protocols. ['edn] is the type for endpoint parameters.
      ['flow] is the type for underlying flows.

      Endpoints allow users to create flows by either connecting directly to a
      remote server or by resolving domain names (with {!connect}). *)

  val connect : ('edn, 'flow) protocol -> 'edn -> (flow, error) result io
  (** [connect p e] is the flow connected to the endpoint [e], using the connect
      function defined by [p]. *)

  val register : ('edn, 'flow) impl -> ('edn, 'flow) protocol
  (** [register i] is the representation of the protocol [i]. [i] must provide a
      [connect] function to allow client flows to be created.

      For instance, on Unix, [Conduit] clients will use [Unix.sockaddr] as flow
      endpoints, while [Unix.file_descr] would be used for the flow transport.

      {[
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
        module Conduit_tcp_tls : sig
          val t : (Unix.sockaddr * Tls.Config.client, Unix.file_descr) protocol
        end = struct
          let t = register Conduit_fd.t (module TLS)
        end
      ]}

      As a protocol implementer, you must {i register} your implementation and
      expose the {i witness} of it. Then, users will be able to use it. *)

  val impl : ('edn, 'flow) protocol -> ('edn, 'flow) impl
  (** [impl t] is the protocol implementation associated to [t]. *)

  (** {2:registration Flow extensions.} *)

  (** [REPR] allows users to extract concrete representation of {!flow} values. *)
  module type REPR = sig
    type t

    type flow += T of t
  end

  type 'a repr = (module REPR with type t = 'a)

  val repr : (_, 'flow) protocol -> 'flow repr
  (** [repr t] is the concrete represenation of the [t].

      It can then be used to destruct {!flow} values, via pattern-matching. For
      instance, to set the underlying file-decriptor as non-blocking, one can
      do:

      {[
        module TCP: Conduit.PROTOCOL with type flow = Unix.file_descr
                                      and type endpoint = Unix.sockaddr

          module Conduit_tcp : sig
            type t = (Unix.sockaddr, Unix.file_descr) Conduit.value

            type Conduit.flow += T of t

            val t : (Unix.sockaddr, Unix.file_descr) protocol
          end = struct
            let t = register (module TCP)

            include (val Conduit.repr t)
          end
      ]}

      With this interface, users are able to {i destruct} {!flow} to your
      concrete type:

      {[
        Conduit.connect domain_name >>? function
          | Conduit_tcp.T (file_descr : Unix.file_descr) -> ...
          | _ -> ...
      ]} *)

  (** {2:resolution Resolvers} *)

  module Endpoint : module type of Endpoint

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

  val resolve : resolvers -> Endpoint.t -> (flow, error) result io
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
          Conduit.resolve resolvers (Conduit.Endpoint.domain mirage_io)
          >>? function
          | TCP.T (Conduit.Value file_descr) as flow ->
              let peer = Unix.getpeername file_descr in
              ignore @@ Conduit.send flow ("Hello " ^ string_of_sockaddr peer)
          | flow -> ignore @@ Conduit.send flow "Hello World!"
      ]} *)

  (** {2:service Services.} *)

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

    type ('cfg, 'server, 'flow) t
    (** The type for services. ['cfg] is the type for configuration, ['server]
        is the type for server states. ['flow] is the type for underlying flows. *)

    val repr : ('cfg, 't, 'flow) t -> 'flow repr

    val register :
      (_, 'flow) protocol ->
      ('cfg, 'server, 'flow) impl ->
      ('cfg, 'server, 'flow) t
    (** [register f i] is the service using the implementation [i] using the
        flow representation [f]. [service] must define [make] and [accept]
        function to be able to create server-side flows.

        For instance:

        {[
          module TCP : SERVICE with type configuration = Unix.sockaddr
                                     and type t = Unix.file_descr
                                     and type flow = Unix.file_descr

          let tcp : (Unix.sockaddr, Unix.file_descr, Unix.file_descr) service =
            Service.register Conduit_fd.t (module TCP)
        ]} *)

    val impl : ('edn, 't, 'flow) t -> ('edn, 't, 'flow) impl
    (** [impl t] is the protocol implementation associated to [t]. *)

    type error = private [> `Msg of string ]

    val pp_error : error Fmt.t

    val init : ('cfg, 't, 'flow) t -> 'cfg -> ('t, error) result io
    (** [init s cfg] initialises the service with the configuration [cfg]. *)

    val accept : ('cfg, 't, 'flow) t -> 't -> (flow, error) result io
    (** [accept s t] waits for a connection on the service [t]. The result is a
        {i flow} connected to the client.

        For instance:

        {[
          let handler (flow : Conduit.flow) =
            Conduit.send flow "Hello World!" >>= fun _ ->
            ...

          let run service cfg =
            Service.init service cfg >>? fun t ->
            let rec loop t =
              Service.accept service t >>? fun flow ->
              async (fun () -> handler flow);
              loop t
            in
            loop t

          let () = run tcp_service (localhost, 8080)
          let () = run tls_service (certs, (localhost, 8080))
        ]} *)

    val stop : ('cfg, 't, 'flow) t -> 't -> (unit, error) result io
    (** [stop s t] releases the resources associated to the server [t]. *)
  end
end
