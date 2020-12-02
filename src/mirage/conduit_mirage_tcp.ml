type ('stack, 'ip) endpoint = {
  stack : 'stack;
  keepalive : Mirage_protocols.Keepalive.t option;
  ip : 'ip;
  port : int;
}

type 'stack configuration = {
  stack : 'stack;
  keepalive : Mirage_protocols.Keepalive.t option;
  port : int;
}

module Make (StackV4 : Mirage_stack.V4) = struct
  open Rresult
  open Lwt.Infix

  module Protocol = struct
    type nonrec endpoint = (StackV4.t, Ipaddr.V4.t) endpoint

    let connect ({ stack; keepalive; ip; port } : endpoint) =
      StackV4.TCPV4.create_connection ?keepalive (StackV4.tcpv4 stack) (ip, port)

    include StackV4.TCPV4
  end

  type protocol = StackV4.TCPV4.flow Conduit_mirage.flow0

  let dst ({ Conduit_mirage.flow; _ } : protocol) = StackV4.TCPV4.dst flow

  let protocol = Conduit_mirage.protocol_of_mirage_flow (module Protocol)

  include (val Conduit_mirage.repr protocol)

  type nonrec configuration = StackV4.t configuration

  type service = {
    stack : StackV4.t;
    queue : StackV4.TCPV4.flow Queue.t;
    condition : unit Lwt_condition.t;
    mutex : Lwt_mutex.t;
    mutable closed : bool;
  }

  module Service = struct
    type +'a io = 'a Conduit_mirage.io

    type error = Connection_aborted

    let pp_error : error Fmt.t =
     fun ppf -> function
      | Connection_aborted -> Fmt.string ppf "Connection aborted"

    type flow = protocol

    type nonrec configuration = configuration

    type t = service

    let init { stack; keepalive; port } =
      let queue = Queue.create () in
      let condition = Lwt_condition.create () in
      let mutex = Lwt_mutex.create () in
      let listener flow =
        Lwt_mutex.lock mutex >>= fun () ->
        Queue.push flow queue ;
        Lwt_condition.signal condition () ;
        Lwt_mutex.unlock mutex ;
        Lwt.return () in
      StackV4.listen_tcpv4 ?keepalive stack ~port listener ;
      Lwt.return (Ok { stack; queue; condition; mutex; closed = false })

    let rec accept ({ queue; condition; mutex; _ } as t) =
      Lwt_mutex.lock mutex >>= fun () ->
      let rec await () =
        if Queue.is_empty queue && not t.closed
        then Lwt_condition.wait condition ~mutex >>= await
        else Lwt.return () in
      await () >>= fun () ->
      match Queue.pop queue with
      | flow ->
          Lwt_mutex.unlock mutex ;
          let flow =
            {
              Conduit_mirage.flow;
              Conduit_mirage.queue =
                Ke.Rke.create
                  ~capacity:(Conduit_mirage.page_size * 2)
                  Bigarray.char;
            } in
          Lwt.return (Ok flow)
      | exception Queue.Empty ->
          if t.closed
          then (
            Lwt_mutex.unlock mutex ;
            Lwt.return (Error Connection_aborted))
          else (
            Lwt_mutex.unlock mutex ;
            accept t)

    let stop ({ stack; mutex; _ } as t) =
      Lwt_mutex.with_lock mutex (fun () ->
          StackV4.disconnect stack >>= fun () ->
          t.closed <- true ;
          Lwt.return (Ok ()))
  end

  let service = Conduit_mirage.Service.register (module Service) protocol

  let configuration stackv4 ?keepalive ~port =
    { stack = stackv4; keepalive; port }
end
