module IO = struct
  type +'a t = 'a Lwt.t

  let bind x f = Lwt.bind x f

  let return x = Lwt.return x
end

module Conduit = Conduit.Make (IO) (Cstruct) (Cstruct)
include Conduit

type ('a, 'b, 'c) service = ('a, 'b, 'c) Service.t

let failwith fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

let io_of_flow flow =
  let open Lwt.Infix in
  let ic_closed = ref false and oc_closed = ref false in
  let close () =
    if !ic_closed && !oc_closed
    then
      close flow >>= function
      | Ok () -> Lwt.return_unit
      | Error err -> failwith "%a" pp_error err
    else Lwt.return_unit in
  let ic_close () =
    ic_closed := true ;
    close () in
  let oc_close () =
    oc_closed := true ;
    close () in
  let rrecv buf off len =
    let raw = Cstruct.of_bigarray buf ~off ~len in
    recv flow raw >>= function
    | Ok (`Input len) -> Lwt.return len
    | Ok `End_of_flow -> Lwt.return 0
    | Error err ->
        ic_closed := true ;
        failwith "%a" pp_error err in
  let ic = Lwt_io.make ~close:ic_close ~mode:Lwt_io.input rrecv in
  let ssend buf off len =
    let raw = Cstruct.of_bigarray buf ~off ~len in
    send flow raw >>= function
    | Ok len -> Lwt.return len
    | Error err ->
        oc_closed := true ;
        failwith "%a" pp_error err in
  let oc = Lwt_io.make ~close:oc_close ~mode:Lwt_io.output ssend in
  (ic, oc)

let ( >>? ) = Lwt_result.bind

let serve :
    type cfg service v.
    ?timeout:int ->
    ?stop:Lwt_switch.t ->
    handler:(flow -> unit Lwt.t) ->
    (cfg, service, v) Service.t ->
    cfg ->
    unit Lwt.t =
 fun ?timeout ?stop ~handler service cfg ->
  let open Lwt.Infix in
  let timeout () =
    match timeout with
    | None -> Lwt.wait () |> fst
    | Some t -> Lwt_unix.sleep (float_of_int t) in
  Service.init service cfg >>= function
  | Error err -> failwith "%a" Service.pp_error err
  | Ok t -> (
      let switched_off =
        let t, u = Lwt.wait () in
        Lwt_switch.add_hook stop (fun () ->
            Lwt.wakeup_later u (Ok `Stopped) ;
            Lwt.return_unit) ;
        t in
      let rec loop () =
        let accept =
          Service.accept service t >>? fun flow -> Lwt.return_ok (`Flow flow)
        in
        Lwt.pick [ accept; (timeout () >|= fun () -> Ok `Timeout) ] >>? function
        | `Flow flow ->
            Lwt.async (fun () -> handler flow) ;
            Lwt.pause () >>= loop
        | `Timeout -> Lwt.return (Ok `Timeout) in
      let stop_result =
        Lwt.pick [ switched_off; loop () ] >>= function
        | Ok (`Timeout | `Stopped) -> Service.stop service t
        | Error _ as err0 -> (
            Service.stop service t >>= function
            | Ok () | Error _ -> Lwt.return err0) in
      stop_result >>= function
      | Ok () -> Lwt.return_unit
      | Error err -> failwith "%a" Service.pp_error err)

module TCP = struct
  open Lwt.Infix

  let pf = Format.fprintf

  let pp_sockaddr ppf = function
    | Unix.ADDR_UNIX v -> pf ppf "<%s>" v
    | Unix.ADDR_INET (inet_addr, port) ->
        pf ppf "<%s:%d>" (Unix.string_of_inet_addr inet_addr) port

  module Protocol = struct
    type input = Cstruct.t

    type output = Cstruct.t

    type +'a io = 'a Lwt.t

    type endpoint = Lwt_unix.sockaddr

    type flow = {
      socket : Lwt_unix.file_descr;
      sockaddr : Lwt_unix.sockaddr;
      linger : Bytes.t;
      mutable closed : bool;
    }

    let peer { sockaddr; _ } = sockaddr

    let sock { socket; _ } = Lwt_unix.getsockname socket

    let file_descr { socket; _ } = socket

    type error =
      [ `Closed_by_peer
      | `Operation_not_permitted
      | `Address_already_in_use of Unix.sockaddr
      | `Cannot_assign_requested_address of Unix.sockaddr
      | `Address_family_not_supported_by_protocol of Unix.sockaddr
      | `Operation_already_in_progress
      | `Bad_address
      | `Network_is_unreachable
      | `Connection_timed_out
      | `Connection_refused
      | `Transport_endpoint_is_not_connected ]

    let pp_error ppf = function
      | `Closed_by_peer -> pf ppf "Connection closed by peer"
      | `Operation_not_permitted -> pf ppf "Operation not permitted"
      | `Address_already_in_use sockaddr ->
          pf ppf "Address %a already in use" pp_sockaddr sockaddr
      | `Cannot_assign_requested_address sockaddr ->
          pf ppf "Cannot assign request address %a" pp_sockaddr sockaddr
      | `Address_family_not_supported_by_protocol sockaddr ->
          pf ppf "Address family %a not supported by protocol" pp_sockaddr
            sockaddr
      | `Operation_already_in_progress -> pf ppf "Operation already in progress"
      | `Bad_address -> pf ppf "Bad address"
      | `Network_is_unreachable -> pf ppf "Network is unreachable"
      | `Connection_timed_out -> pf ppf "Connection timed out"
      | `Connection_refused -> pf ppf "Connection refused"
      | `Transport_endpoint_is_not_connected ->
          pf ppf "Transport endpoint is not connected"

    let io_buffer_size = 65536

    let connect sockaddr =
      let socket =
        Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
      in
      let linger = Bytes.create io_buffer_size in
      let rec go () =
        let process () =
          Lwt_unix.connect socket sockaddr >>= fun () ->
          Lwt.return_ok { socket; sockaddr; linger; closed = false } in
        Lwt.catch process @@ function
        | Unix.(Unix_error ((EACCES | EPERM), _, _)) ->
            Lwt.return_error `Operation_not_permitted
        | Unix.(Unix_error (EADDRINUSE, _, _)) ->
            Lwt.return_error (`Address_already_in_use sockaddr)
        | Unix.(Unix_error (EADDRNOTAVAIL, _, _)) ->
            Lwt.return_error (`Cannot_assign_requested_address sockaddr)
        | Unix.(Unix_error (EAFNOSUPPORT, _, _)) ->
            Lwt.return_error
              (`Address_family_not_supported_by_protocol sockaddr)
        | Unix.(Unix_error (EALREADY, _, _)) ->
            Lwt.return_error `Operation_already_in_progress
        | Unix.(Unix_error (EFAULT, _, _)) -> Lwt.return_error `Bad_address
        | Unix.(Unix_error (ENETUNREACH, _, _)) ->
            Lwt.return_error `Network_is_unreachable
        | Unix.(Unix_error (ETIMEDOUT, _, _)) ->
            Lwt.return_error `Connection_timed_out
        | Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> go ()
        | Unix.(Unix_error (EINTR, _, _)) -> go ()
        | Unix.(Unix_error (ECONNREFUSED, _, _)) ->
            Lwt.return_error `Connection_refused
        | exn -> Lwt.fail exn
        (* | EPROTOTYPE: impossible *)
        (* | EISCONN: impossible *)
        (* | ENOTSOCK: impossible *)
        (* | EBADF: impossible *)
        (* | EINPROGRESS: TODO *) in
      go ()

    let rec recv ({ socket; closed; _ } as t)
        ({ Cstruct.buffer; off; len } as raw) =
      if closed
      then Lwt.return_ok `End_of_flow
      else
        let process () =
          Lwt_bytes.read socket buffer off len >>= function
          | 0 ->
              Lwt_unix.shutdown socket SHUTDOWN_RECEIVE ;
              Lwt.return (Ok `End_of_flow)
          | len -> Lwt.return (Ok (`Input len)) in
        Lwt.catch process @@ function
        | Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> recv t raw
        | Unix.(Unix_error (EINTR, _, _)) -> recv t raw
        | Unix.(Unix_error (EFAULT, _, _)) -> Lwt.return_error `Bad_address
        | Unix.(Unix_error (ENOTCONN, _, _)) ->
            Lwt.return_error `Transport_endpoint_is_not_connected
        (* | Unix.(Unix_error (ECONNREFUSED, _, _)): TODO *)
        (* | EBADF: impossible *)
        | exn -> Lwt.fail exn

    let rec send ({ socket; closed; _ } as t)
        ({ Cstruct.buffer; off; len } as raw) =
      if closed
      then Lwt.return_error `Closed_by_peer
      else
        let process () =
          Lwt_bytes.write socket buffer off len >|= fun len -> Ok len in
        Lwt.catch process @@ function
        | Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> send t raw
        | Unix.(Unix_error (EINTR, _, _)) -> send t raw
        | Unix.(Unix_error (EACCES, _, _)) ->
            Lwt.return_error `Operation_not_permitted
        | Unix.(Unix_error (ECONNRESET, _, _)) ->
            Lwt_unix.shutdown socket SHUTDOWN_SEND ;
            Lwt.return_error `Closed_by_peer
        | Unix.(Unix_error (EPIPE, _, _)) ->
            Lwt_unix.shutdown socket SHUTDOWN_SEND ;
            Lwt.return_error `Closed_by_peer
        | Unix.(Unix_error (EDESTADDRREQ, _, _))
        | Unix.(Unix_error (ENOTCONN, _, _)) ->
            Lwt.return_error `Transport_endpoint_is_not_connected
        | Unix.(Unix_error (EFAULT, _, _)) -> Lwt.return_error `Bad_address
        (* ENOTSOCK: impossible *)
        (* EISCONN: TODO *)
        (* EOPNOTSUPP: TODO *)
        (* ENOBUFS: TODO & impossible into Linux *)
        | exn -> Lwt.fail exn

    let rec close t =
      let process () =
        if not t.closed
        then (
          Lwt_unix.close t.socket >>= fun () ->
          t.closed <- true ;
          Lwt.return_ok ())
        else Lwt.return_ok () in
      Lwt.catch process @@ function
      | Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> close t
      | Unix.(Unix_error (EINTR, _, _)) -> close t
      | exn -> Lwt.fail exn
  end

  type configuration = { sockaddr : Lwt_unix.sockaddr; capacity : int }

  module Service = struct
    type +'a io = 'a Lwt.t

    type nonrec configuration = configuration = {
      sockaddr : Lwt_unix.sockaddr;
      capacity : int;
    }

    type t = Lwt_unix.file_descr

    type flow = Protocol.flow

    type error =
      [ `Address_is_protected of Unix.sockaddr
      | `Operation_not_permitted of Unix.sockaddr
      | `Address_already_in_use of Unix.sockaddr
      | `Address_is_not_valid of Unix.sockaddr
      | `Cannot_assign_requested_address of Unix.sockaddr
      | `Bad_address
      | `Too_many_symbolic_links of Unix.sockaddr
      | `Name_too_long of Unix.sockaddr
      | `Operation_not_supported
      | `Limit_reached
      | `Protocol_error
      | `Firewall_rules_forbid_connection ]

    let pp_error ppf = function
      | `Address_is_protected sockaddr ->
          pf ppf "Address %a is protected" pp_sockaddr sockaddr
      | `Operation_not_permitted sockaddr ->
          pf ppf "Operation on %a is not permitted" pp_sockaddr sockaddr
      | `Address_already_in_use sockaddr ->
          pf ppf "Address %a already in use" pp_sockaddr sockaddr
      | `Address_is_not_valid sockaddr ->
          pf ppf "Address %a is not valid" pp_sockaddr sockaddr
      | `Cannot_assign_requested_address sockaddr ->
          pf ppf "Cannot assign request address %a" pp_sockaddr sockaddr
      | `Bad_address -> pf ppf "Bad address"
      | `Too_many_symbolic_links sockaddr ->
          pf ppf "Too many symbolic links on %a" pp_sockaddr sockaddr
      | `Name_too_long sockaddr ->
          pf ppf "Name %a too long" pp_sockaddr sockaddr
      | `Operation_not_supported -> pf ppf "Operation not supported"
      | `Limit_reached -> pf ppf "Limit of file-descriptors reached"
      | `Protocol_error -> pf ppf "Protocol error"
      | `Firewall_rules_forbid_connection ->
          pf ppf "Firewill rules forbid connection"

    let is_addr_inet = function
      | Unix.ADDR_INET _ -> true
      | Unix.ADDR_UNIX _ -> false

    let init { sockaddr; capacity } =
      let socket =
        Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0
      in
      Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true ;
      let process () =
        Lwt_unix.bind socket sockaddr >>= fun () ->
        Lwt_unix.listen socket capacity ;
        Lwt.return_ok socket in
      Lwt.catch process @@ function
      (* bind *)
      | Unix.(Unix_error (EACCES, _, _)) when is_addr_inet sockaddr ->
          Lwt.return_error (`Address_is_protected sockaddr)
      | Unix.(Unix_error (EACCES, _, _)) (* when is_addr_unix sockaddr *) ->
          Lwt.return_error (`Operation_not_permitted sockaddr)
      | Unix.(Unix_error (EADDRINUSE, _, _)) ->
          Lwt.return_error (`Address_already_in_use sockaddr)
      | Unix.(Unix_error (EINVAL, _, _)) ->
          Lwt.return_error (`Address_is_not_valid sockaddr)
      (* | ENOTSOCK: impossible *)
      | Unix.(Unix_error (EADDRNOTAVAIL, _, _)) ->
          Lwt.return_error (`Cannot_assign_requested_address sockaddr)
      | Unix.(Unix_error (EFAULT, _, _)) -> Lwt.return_error `Bad_address
      | Unix.(Unix_error (ELOOP, _, _)) ->
          Lwt.return_error (`Too_many_symbolic_links sockaddr)
      | Unix.(Unix_error (ENAMETOOLONG, _, _)) ->
          Lwt.return_error (`Name_too_long sockaddr)
      (* listen *)
      (* | Unix.(Unix_error (EADDRINUSE, _, _)) -> *)
      | Unix.(Unix_error (EOPNOTSUPP, _, _)) ->
          Lwt.return_error `Operation_not_supported
      | exn -> Lwt.fail exn

    let rec accept service =
      let process () =
        Lwt_unix.accept service >>= fun (socket, sockaddr) ->
        let linger = Bytes.create 0x1000 in
        Lwt.return_ok { Protocol.socket; sockaddr; linger; closed = false }
      in
      Lwt.catch process @@ function
      | Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> accept service
      | Unix.(Unix_error (EINTR, _, _)) -> accept service
      | Unix.(Unix_error (EMFILE, _, _))
      | Unix.(Unix_error ((ENOBUFS | ENOMEM), _, _)) ->
          Lwt.return_error `Limit_reached
      | Unix.(Unix_error (EPROTOTYPE, _, _)) -> Lwt.return_error `Protocol_error
      | Unix.(Unix_error (EPERM, _, _)) ->
          Lwt.return_error `Firewall_rules_forbid_connection
      | exn -> Lwt.fail exn

    let stop _service =
      (* XXX(dinosaure): it seems that on MacOS, try to close the [master]
         socket raises an error. *)
      Lwt.return_ok ()
  end

  let protocol = register (module Protocol)

  include (val repr protocol)

  let service = Conduit.Service.register (module Service) protocol

  let configuration ?(capacity = 40) sockaddr = { capacity; sockaddr }

  let resolve ~port = function
    | Conduit.Endpoint.IP ip ->
        Lwt.return_some (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ip, port))
    | Conduit.Endpoint.Domain domain_name -> (
        Lwt_unix.gethostbyname (Domain_name.to_string domain_name) >>= function
        | { Unix.h_addr_list; _ } when Array.length h_addr_list > 0 ->
            Lwt.return_some (Unix.ADDR_INET (h_addr_list.(0), port))
        | _ -> Lwt.return_none)
end
