### ping-pong tests

`ping-pong` wants to test `conduit-lwt-unix`. The process to test it is:
- we start a server which respond with "ping" if it receives "pong" and vice-versa
- we launch many clients to communicate with it

Currently, `ping-pong` tests:
- a simple TCP/IP server/clients
- a TLS + TCP/IP server/clients
- a SSL + TCP/IP server/clients

All of these share the same server and the same client implementation. The test shows to
us that the logic of the server/client is independent from the protocol used.

Finally, where all clients are finished, we stop the server.

### Async tests

`with_async` does the same job as `ping_pong` and it ~is~ implemented in the same way than
`ping_pong` but with `async`. The test does not take the advantage of `Reader.t` or `Writer.t`
due to the non-atomicity of `Conduit_async_tls.Protocol.{recv,send}` (see `conduit-tls` for
more details). So we re-use a `getline` implementation as `ping_pong`.

### Results

The test wants to show that these programs terminate correctly!
