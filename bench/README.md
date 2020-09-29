## Cost - a little benchmark about injection/projection

When the user does:
```ocaml
Conduit.connect edn protocol >>= fun flow ->
Conduit.send flow str
```

Internally, `conduit` uses an `Hashtbl.t` to get the
protocol implementation. You can see it into [lib/e0.ml].

If performances matters, the user can get the protocol
implementation one time with `Conduit.flow`:
```ocaml
Conduit.connect edn protocol >>= fun flow ->
let Conduit.Flow (flow, (module Flow)) = Conduit.flow flow in
Flow.send flow str
```

To ensure a small overhead between the first case and the
second case, `conduit` provides a little benchmark to see
the difference:
```sh
$ dune exec bench/cost.exe
with Conduit:		252.20ns (r²: 0.99).
without Conduit:	215.03ns (r²: 0.99).
Overhead:		37.17ns.
```

And check that:
- the overhead is stable regardless the number of protocol
  implementations available into the global `Hashtbl.t`
- the overhead is small enough not to have performance
  regression

### A use-pattern

To be fast about projection of the protocol implementation,
`conduit` tweak a bit the implementation of the `Hashtbl.t`
and it proposes a memoization of the last call of `Ptr.prj`.

If you call several times `Conduit.send`/`Conduit.recv` with
the same flow value, we directly load the protocol
implementation kept into a extra mutable field of the `Hashtbl.t`.
