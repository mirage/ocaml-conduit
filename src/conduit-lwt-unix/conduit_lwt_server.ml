open Lwt.Infix

let src = Logs.Src.create "conduit_lwt_server" ~doc:"Conduit Lwt transport"

module Log = (val Logs.src_log src : Logs.LOG)

let safe_close t =
  Lwt.catch (fun () -> Lwt_io.close t) (fun _ -> Lwt.return_unit)

let close (ic, oc) = safe_close oc >>= fun () -> safe_close ic

let with_socket sockaddr f =
  let fd =
    Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
  in
  Lwt.catch
    (fun () -> f fd)
    (fun e ->
      Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)
      >>= fun () -> Lwt.fail e)

let listen ?(backlog = 128) sa =
  with_socket sa (fun fd ->
      Lwt_unix.(setsockopt fd SO_REUSEADDR true);
      Lwt_unix.bind fd sa >|= fun () ->
      Lwt_unix.listen fd backlog;
      Lwt_unix.set_close_on_exec fd;
      fd)

let process_accept ?timeout callback (sa, ic, oc) =
  let c = callback sa ic oc in
  let events =
    match timeout with
    | None -> [ c ]
    | Some t -> [ c; Lwt_unix.sleep (float_of_int t) ]
  in
  Lwt.finalize (fun () -> Lwt.pick events) (fun () -> close (ic, oc))

(* File descriptors are a global resource so this has to be a global limit too *)
let maxactive = ref None
let active = ref 0
let cond = Lwt_condition.create ()
let connected () = incr active

let disconnected () =
  decr active;
  Lwt_condition.broadcast cond ()

let rec throttle () =
  match !maxactive with
  | Some limit when !active > limit -> Lwt_condition.wait cond >>= throttle
  | _ -> Lwt.return_unit

let set_max_active max_active =
  maxactive := Some max_active;
  Lwt_condition.broadcast cond ()

let run_handler handler v =
  Lwt.async (fun () ->
      Lwt.try_bind
        (fun () -> handler v)
        (fun () ->
          disconnected ();
          Lwt.return_unit)
        (fun x ->
          disconnected ();
          (match x with
          | Lwt.Canceled -> ()
          | ex ->
              Log.warn (fun f ->
                  f "Uncaught exception in handler: %s" (Printexc.to_string ex)));
          Lwt.return_unit))

let init ?(nconn = 10_000) ?(stop = fst (Lwt.wait ())) handler fd =
  let stop = Lwt.map (fun () -> `Stop) stop in
  let log_exn = function
    | Some ex ->
        Log.warn (fun f ->
            f "Uncaught exception accepting connection: %s"
              (Printexc.to_string ex))
    | None -> ()
  in
  let rec loop () =
    let accepted =
      Lwt_unix.accept_n fd nconn >>= fun (connections, maybe_error) ->
      log_exn maybe_error;
      Lwt.return (`Accepted connections)
    in
    Lwt.catch
      (fun () ->
        Lwt.choose [ accepted; stop ] >>= function
        | `Stop ->
            Lwt.cancel accepted;
            Lwt.return_unit
        | `Accepted connections ->
            Lwt_list.iter_s
              (fun v ->
                connected ();
                throttle () >>= fun () ->
                run_handler handler v;
                Lwt.return_unit)
              connections
              >>= Lwt.pause
            >>= loop)
      (function
        | Lwt.Canceled -> Lwt.return_unit
        | ex ->
            log_exn (Some ex);
            Lwt.pause () >>= loop)
  in
  Lwt.finalize loop (fun () -> Lwt_unix.close fd)
