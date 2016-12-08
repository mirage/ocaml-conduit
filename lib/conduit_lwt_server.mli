
val close : 'a Lwt_io.channel * 'b Lwt_io.channel -> unit Lwt.t

val listen : ?backlog:int -> Unix.sockaddr -> Lwt_unix.file_descr

val with_socket
  : Unix.sockaddr
  -> (Lwt_unix.file_descr -> 'a Lwt.t)
  -> 'a Lwt.t

val process_accept
  : ?timeout:int
  -> ('a -> 'b Lwt_io.channel -> 'c Lwt_io.channel -> unit Lwt.t)
  -> 'a * 'b Lwt_io.channel * 'c Lwt_io.channel
  -> unit

val init
  : ?stop:unit Lwt.t
  -> (Lwt_unix.file_descr * Lwt_unix.sockaddr -> unit)
  -> Lwt_unix.file_descr
  -> unit Lwt.t
