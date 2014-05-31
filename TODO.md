- Expose some connection information (e.g. source IP) so that
  libraries upstream such as Cohttp can query it.

- Lwt_conduit needs a non-Unix functor for Mirage to use as well.

- The source inet addr needs to be a list for dual-stack to work well.
