(** An implementation of [conduit] according the interface [Mirage_flow.S]. This
    module is {b deprecated} when the current implementation of [read] has
    another behaviour:

    [conduit] provides:

    {[ val read : flow -> Cstruct.t -> (int or_eoi, error) result Lwt.t ]}

    where [mirage-flow] expects:

    {[ val read : flow -> (Cstruct.t or_eoi, error) result Lwt.t ]}

    This current implementation allocates an {b arbitrary} 4096 bytes buffer to
    fit under the [mirage-flow] interface. [conduit] did the choice to follow
    the POSIX interface and let the end-user to allocate by himself the input
    buffer. *)

val page_size : int
(** [page_size = 4096] *)

include Mirage_flow.S with type flow = Conduit_mirage.flow
