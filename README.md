[![Build Status](https://travis-ci.org/mirage/ocaml-conduit.svg?branch=master)](https://travis-ci.org/mirage/ocaml-conduit)

## OCaml network conduit library

The `conduit` library takes care of establishing and listening for 
TCP and SSL/TLS connections for the Lwt and Async libraries.

The reason this library exists is to provide a degree of abstraction
from the precise SSL library used, since there are a variety of ways
to bind to a library (e.g. the C FFI, or the Ctypes library), as well
as well as which library is used (just OpenSSL for now).

### Modules

Source code is in `lib/`.

* `Lwt_unix_conduit` has the Lwt UNIX modules.
* `Async_conduit` has the Core/Async modules.

### Further Informartion

* **WWW:** https://github.com/mirage/ocaml-conduit
* **E-mail:** <mirageos-devel@lists.xenproject.org>
* **Bugs:** https://github.com/mirage/ocaml-conduit/issues
