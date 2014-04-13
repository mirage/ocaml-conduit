- Expose some connection information (e.g. source IP) so that
  libraries upstream such as Cohttp can query it.

- Perhaps generalise into a logger, although that can also be
  in a separate lib, but could be convenient here.

- Lwt_conduit needs a non-Unix functor for Mirage to use as well.


