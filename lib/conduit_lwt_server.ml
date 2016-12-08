open Lwt.Infix

let safe_close t =
  Lwt.catch
    (fun () -> Lwt_io.close t)
    (fun _ -> Lwt.return_unit)

let close (ic, oc) =
  safe_close oc >>= fun () ->
  safe_close ic

let listen ?(backlog=128) sa =
  let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sa) Unix.SOCK_STREAM 0 in
  Lwt_unix.(setsockopt fd SO_REUSEADDR true);
  Lwt_unix.bind fd sa;
  Lwt_unix.listen fd backlog;
  Lwt_unix.set_close_on_exec fd;
  fd

let with_socket sockaddr f =
  let fd =
    Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
  Lwt.catch (fun () -> f fd) (fun e ->
      Lwt.catch
        (fun () -> Lwt_unix.close fd)
        (fun _ -> Lwt.return_unit)
      >>= fun () ->
      Lwt.fail e)

let process_accept ~timeout callback (sa,ic,oc) =
  let c = callback sa ic oc in
  let events = match timeout with
    | None -> [c]
    | Some t -> [c; (Lwt_unix.sleep (float_of_int t)) ] in
  Lwt.finalize (fun () -> Lwt.pick events) (fun () -> close (ic,oc))
  |> Lwt.ignore_result
