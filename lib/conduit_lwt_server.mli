
val close : 'a Lwt_io.channel * 'b Lwt_io.channel -> unit Lwt.t

val with_socket
  : Unix.sockaddr
  -> (Lwt_unix.file_descr -> 'a Lwt.t)
  -> 'a Lwt.t

val process_accept
  : timeout:int option
  -> ('a -> 'b Lwt_io.channel -> 'c Lwt_io.channel -> unit Lwt.t)
  -> 'a * 'b Lwt_io.channel * 'c Lwt_io.channel
  -> unit
