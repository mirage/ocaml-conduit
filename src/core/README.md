## Conduit - core library

The main goal of `conduit` is to be able to use an abstract type flow as a
representation of a `socket` independently of the implementation.

Of course, this case appears on MirageOS where the implementation depends on the
_target_. But it can be more general where, as a library, you should not depends
on a given implementation of a protocol. In such context, you are able to
implement a way to communicate with a peer without a full knowledge of the
underlying protocol used.

By this abstraction, protocol implementer can _compose_ the protocol with an
other layer such as TLS and still be able to provide the same interface.

`conduit` wants to provide a common way to start a server too. This feature is
less abstracted than the communication with a peer but it provides a better
interface than before.

### Implementation and `resolvers`

Conduit splits the knowledge of protocols into 2 elements:
- a global `Hashtbl.t`
- a local `resolvers`

A protocol must be registered with `register_protocol`:
```ocaml
let witness = Conduit.register_protocol ~key ~protocol:(module Protocol)
```

A protocol must follow an interface described by `PROTOCOL`. The implementer
must create a new `key` with `Conduit.key`.

The _witness_ can be ignored and hidden. However, it should be properly exposed
with the protocol to help the end-user to enforce `conduit` to use this specific
protocol.

The registration fills the internal global `Hashtbl` of `conduit`. Even if this
implementation is available into `conduit`, it's not true that `conduit` will
systematically use it (it's the main difference with the old version of
`conduit`). However, the _key_ used to register your protocol must exposed,
otherwise your protocol will never be available with `conduit`.

In fact, the registration needs a `key` which is a _witness_ of needed value to
_initialize_ a flow according your protocol implementation. For example, an
`Unix.socket` must need a `Unix.socket_domain` to be created. The type of this
value will be a part of the _witness_ `key`.

By this way, registration of a protocol must be done like this:
```ocaml
let key = Conduit.key "my-protocol"
let witness = Conduit.register_protocol ~key ~protocol:(module Protocol)
```

Then, `key` must be exposed to the end-user to be able to fill `resolvers`.

#### As the end-user wants

So even if your protocol is well registered into `conduit`, the end-user still
is able to use or ignore it. The existence into the resolution process of
`conduit` of your protocol only exists if the end-user fill the given
`resolvers` with your `key`.

By this way, it is on the responsibility of the end-user to properly create
needed values by the _initialization_ of the flow according your protocol
implementation.

```ocaml
(* we assume a TCP/IP protocol imported by a library. *)
val key : Unix.inet_addr Conduit.key
val witness : Unix.file_descr Conduit.Witness.protocol

let resolve domain_name : Unix.inet_addr option = Unix.gethostbyname domain_name
let resolvers = Conduit.register_resolver ~key resolve Conduit.empty

let _ =
  Conduit.flow resolvers domain_name >>= fun flow -> ...
```

In your example, others protocols can be registered such as SSH or TCP + TLS,
however, the end-user registered into the `resolvers` only the TCP protocol.
Such example shows that the end-user can restrict the resolution on few
protocols like secured protocols.

This new way to start a connection lets the end-user to specify:
- which protocol he wants to use
- how such protocol can be created 
- which resolves the domain-name

Usually, the third point is a call to `gethostbyname` which trusts on your
`/etc/resolv.conf` but such service does not exist into a MirageOS world. So
`conduit` gives the ability to specify which service handles that.

The second point is the most important where it lets the user to specify a
process/function to _initialize_ a communication. For example, the TLS stack
expects an _authenticator_ which verifies the given certificate by your peer -
the user is able to specify an _authenticator_ which trusts on a specifc chain
of certificates.

The first point is to let the user to enforce a protocol. Instead to try several
of them in order to their priorities, the user can enforce to use a special one.

### Create a new flow

As an implementer of a protocol, the way to create a /flow/ differs for each
protocols. We said that an `Unix.socket` needs a `Unix.socket_domain` to be
created. However, it's not the case for a TLS flow which should need a
`Tls.Config.t` (or basically something more complex).

At the end, `conduit` lets the end-user to create this kind of value used then
to properly create a `flow`. Finally `conduit` has the ability to let the
implementer to define the type of this required value.

In your previous example it's our `resolve` function.

The rule is easy, for N `key`, the end-user should (but it's not mandatory)
define N `resolve` functions. A registration of them into a `resolvers` element
will let `conduit` to try to initiate a _flow_ to the associated protocol - this
association is done by the registration of the protocol between the `key` and
the implementation.

At the end, the process of the resolution is clear:
```
[ `host ] Domain_name.t -> 'edn -> 'flow -> Conduit.flow
```

Where `'edn` is specified by the `key` and `'flow`, by the protocol. The
end-user must implement a function `resolver : [ host ] Domain_name.t` and the
implementer must provide a function `flow : 'edn -> 'flow`. Then, `conduit` does
the glue between them to provide a fully-abstract `Conduit.flow`.

### How to use the `flow`

As an abstracted value, the returned `flow` can be use by:
- `Conduit.recv`
- `Conduit.send`
- `Conduit.close`

NOTE: semantic of them depends on the implementation used by `conduit`.

Internally, `conduit` _extracts_ your `flow` and infer the proper implementation
associated. Then, it uses this implementation registered into our internal
global `Hashtbl.t`.

In other words, a `flow` created by our TCP/IP implementation stack will be
associated to this implementation as long as it exists.

### Provide something more than `PROTOCOL`

It appears that some protocols want to expose more functions that what
`PROTOCOL` defines. By this fact, `conduit` should able to expose such
functions. With the _witness_ given by the registration of the protocol, the
end-user has the ability to extract by himself the real underlying flow.

For example, a TCP/IP `flow` can returns some information such as the IP and
port where it is connected. With the _witness_ of the TCP/IP protocol, we are
able to extract the underlying `Unix.file_descr` (considering as is) and use
directly `Unix.*` functions.

```ocaml
let peer = match Conduit.is flow witness with
  | Some socket -> Unix.getpeername socket
  | None -> failwith "It's not an Unix TCP/IP connection"
```

A layer such as TLS can expose such accessors too like:
```ocaml
type 'flow with_tls

val underlying : 'flow with_tls -> 'flow
val handshake : 'flow with_tls -> bool
```

The end-user has several ways to extract structural `flow` from the abstracted one.
