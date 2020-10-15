## Conduit - a framework to abstract the protocol

The `conduit` library is a simple framework to abstract a _protocol_. It
permits, for a protocol implementer, to get rid the responsibility of the
protocols choice (such as the TLS implementation). So, it provides a degree of
abstraction from precise protocol (like TLS or TCP/IP) libraries used - since
there are a variety of them.

### Documentation & Tutorials

The documentation is available [here][doc].

A simple HOW-TO to describe how to implement a ping-pong server/client and how
to upgrade them with TLS is available [here][howto]

A more complete (but less easier to understand) document to describe Conduit is
available [here][readme].

### Distribution

Conduit comes with several packages:
- `conduit`: the core library which has only 3 dependencies
- `conduit-tls`: a Conduit-compatible [ocaml-tls][ocaml-tls] implementation
- `conduit-lwt`: the library with [lwt][lwt], it provides a Conduit-compatible
  Host's TCP/IP protocol
- `conduit-async`: the library with [async][async], it provides a
  Conduit-compatible Host's TCP/IP protocol
- `conduit-lwt-{ssl,tls}` provides the Host's TCP/IP protocol with SSL (OpenSSL)
  and TLS (`ocaml-tls`)
- `conduit-async-{ssl,tls}` provides the Host's TCP/IP protocol with SSL
  (OpenSSL) and TLS (`ocaml-tls`)
- `conduit-mirage` a Conduit-compatible [mirage-tcpip][mirage-tcpip] protocol

### Further Informartion

* **API Docs:** http://mirage.github.io/ocaml-conduit/
* **WWW:** https://github.com/mirage/ocaml-conduit
* **E-mail:** <mirageos-devel@lists.xenproject.org>
* **Bugs:** https://github.com/mirage/ocaml-conduit/issues

[doc]: https://mirage.github.io/ocaml-conduit/conduit/index.html
[howto]: https://mirage.github.io/ocaml-conduit/conduit/howto.html
[readme]: https://mirage.github.io/ocaml-conduit/conduit/readme.html
[ocaml-tls]: https://github.com/mirleft/ocaml-tls
[lwt]: https://github.com/ocsigen/lwt
[async]: https://github.com/janestreet/async
[mirage-tcpip]: https://github.com/mirage/mirage-tcpip
