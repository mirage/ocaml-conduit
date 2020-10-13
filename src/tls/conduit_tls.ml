module Ke = Ke.Rke

let option_fold ~none ~some = function Some x -> some x | None -> none

(* NOTE(dinosaure): we use an unbound queue where TLS can produce
   something bigger than the given input. It seems hard to limit
   the internal queue and arbitrary limit (like a queue two times
   larger than the input) is not good. By this fact, we use [Ke.Rke]
   even if it an infinitely grow. *)

module Make
    (IO : Conduit.IO)
    (Conduit : Conduit.S
                 with type input = Cstruct.t
                  and type output = Cstruct.t
                  and type +'a io = 'a IO.t) =
struct
  let return x = IO.return x

  let ( >>= ) x f = IO.bind x f

  let ( >>| ) x f = x >>= fun x -> return (f x)

  let ( >>? ) x f =
    x >>= function Ok x -> f x | Error err -> return (Error err)

  let reword_error : ('e0 -> 'e1) -> ('a, 'e0) result -> ('a, 'e1) result =
   fun f -> function Ok v -> Ok v | Error err -> Error (f err)

  let src = Logs.Src.create "conduit-tls"

  module Log = (val Logs.src_log src : Logs.LOG)

  type 'flow protocol_with_tls = {
    mutable tls : Tls.Engine.state option;
    mutable closed : bool;
    raw : Cstruct.t;
    flow : 'flow;
    queue : (char, Bigarray.int8_unsigned_elt) Ke.t;
  }

  let underlying { flow; _ } = flow

  let handshake { tls; _ } =
    match tls with
    | Some tls -> Tls.Engine.handshake_in_progress tls
    | None -> false

  module Make_protocol (Flow : Conduit.PROTOCOL) = struct
    type input = Conduit.input

    type output = Conduit.output

    type +'a io = 'a Conduit.io

    type endpoint = Flow.endpoint * Tls.Config.client

    type flow = Flow.flow protocol_with_tls

    type error =
      [ `Msg of string
      | `Flow of Flow.error
      | `TLS of Tls.Engine.failure
      | `Closed_by_peer ]

    let pp_error : error Fmt.t =
     fun ppf -> function
      | `Msg err -> Fmt.string ppf err
      | `Flow err -> Flow.pp_error ppf err
      | `TLS failure -> Fmt.string ppf (Tls.Engine.string_of_failure failure)
      | `Closed_by_peer -> Fmt.string ppf "Closed by peer"

    let flow_error err = `Flow err

    let flow_wr_opt :
        Flow.flow -> Cstruct.t option -> (unit, error) result Conduit.io =
     fun flow -> function
      | None -> return (Ok ())
      | Some raw ->
          Log.debug (fun m -> m "~> Send %d bytes" (Cstruct.len raw)) ;
          let rec go raw =
            Flow.send flow raw >>| reword_error flow_error >>? fun len ->
            let raw = Cstruct.shift raw len in
            if Cstruct.len raw = 0 then return (Ok ()) else go raw in
          go raw

    let blit src src_off dst dst_off len =
      let src = Cstruct.to_bigarray src in
      Bigstringaf.blit src ~src_off dst ~dst_off ~len

    let queue_wr_opt queue = function
      | None -> ()
      | Some raw ->
          Log.debug (fun m ->
              m "Fill the queue with %d byte(s)." (Cstruct.len raw)) ;
          Ke.N.push queue ~blit ~length:Cstruct.len ~off:0 raw

    let handle_tls :
        Tls.Engine.state ->
        (char, Bigarray.int8_unsigned_elt) Ke.t ->
        Flow.flow ->
        Cstruct.t ->
        (Tls.Engine.state option, error) result IO.t =
     fun tls queue flow raw ->
      match Tls.Engine.handle_tls tls raw with
      | `Fail (failure, `Response resp) ->
          Log.debug (fun m -> m "|- TLS state: Fail") ;
          flow_wr_opt flow (Some resp) >>? fun () ->
          return (Error (`TLS failure))
      | `Ok (`Alert _alert, `Response resp, `Data data) ->
          Log.debug (fun m -> m "|- TLS state: Alert") ;
          queue_wr_opt queue data ;
          flow_wr_opt flow resp >>? fun () -> return (Ok (Some tls))
      | `Ok (`Eof, `Response resp, `Data data) ->
          Log.debug (fun m -> m "|- TLS state: EOF") ;
          queue_wr_opt queue data ;
          flow_wr_opt flow resp >>? fun () -> return (Ok None)
      | `Ok (`Ok tls, `Response resp, `Data data) ->
          (* XXX(dinosaure): it seems that decoding TLS inputs can produce
             something bigger than expected. For example, decoding 4096 bytes
             can produce 4119 byte(s). *)
          Log.debug (fun m -> m "|- TLS state: Ok.") ;
          queue_wr_opt queue data ;
          flow_wr_opt flow resp >>? fun () -> return (Ok (Some tls))

    let handle_handshake :
        Tls.Engine.state ->
        (char, Bigarray.int8_unsigned_elt) Ke.t ->
        Flow.flow ->
        Cstruct.t ->
        (Tls.Engine.state option, error) result IO.t =
     fun tls queue flow raw0 ->
      let rec go tls raw1 =
        match Tls.Engine.can_handle_appdata tls with
        | true ->
            Log.debug (fun m -> m "Start to talk with TLS (handshake is done).") ;
            handle_tls tls queue flow raw1
        | false -> (
            assert (Tls.Engine.handshake_in_progress tls = true) ;
            Log.debug (fun m -> m "Process TLS handshake.") ;

            (* XXX(dinosaure): assertion, [Tls.Engine.handle_tls] consumes all
               bytes of [raw1] and [raw1] is physically a subset of [raw0] (or
               is [raw0]). we can re-use [raw0] for [Flow.recv] safely. *)
            match Tls.Engine.handle_tls tls raw1 with
            | `Ok (`Ok tls, `Response resp, `Data data) ->
                Log.debug (fun m ->
                    m "-- TLS state: OK (data: %d byte(s))"
                      (option_fold ~none:0 ~some:Cstruct.len data)) ;
                queue_wr_opt queue data ;
                flow_wr_opt flow resp >>? fun () ->
                if Tls.Engine.handshake_in_progress tls
                then (
                  Log.debug (fun m ->
                      m "<- Read the TLS flow (while handshake).") ;
                  Flow.recv flow raw0 >>| reword_error flow_error >>? function
                  | `End_of_flow ->
                      Log.warn (fun m ->
                          m
                            "Got EOF from underlying connection while \
                             handshake.") ;
                      return (Ok None)
                  | `Input 0 ->
                      Log.debug (fun m ->
                          m "Underlying connection asks to re-schedule.") ;
                      return (Ok (Some tls))
                  | `Input len ->
                      Log.debug (fun m ->
                          let uid =
                            Hashtbl.hash
                              (Cstruct.to_string (Cstruct.sub raw0 0 len)) in
                          m
                            "<~ [%04x] Got %d bytes (handshake in progress: \
                             true)."
                            uid len) ;
                      go tls (Cstruct.sub raw0 0 len))
                else (
                  Log.debug (fun m -> m "Handshake is done.") ;
                  return (Ok (Some tls)))
            | `Ok (`Eof, `Response resp, `Data data) ->
                Log.debug (fun m -> m "-- TLS state: EOF") ;
                queue_wr_opt queue data ;
                flow_wr_opt flow resp >>? fun () -> return (Ok None)
            | `Fail (failure, `Response resp) ->
                Log.debug (fun m -> m "-- TLS state: Fail") ;
                flow_wr_opt flow (Some resp) >>? fun () ->
                return (Error (`TLS failure))
            | `Ok (`Alert _alert, `Response resp, `Data data) ->
                Log.debug (fun m -> m "-- TLS state: Alert") ;
                queue_wr_opt queue data ;
                flow_wr_opt flow resp >>? fun () -> return (Ok (Some tls)))
      in
      go tls raw0

    let connect (edn, config) =
      Flow.connect edn >>| reword_error flow_error >>? fun flow ->
      let raw = Cstruct.create 0x1000 in
      let queue = Ke.create ~capacity:0x1000 Bigarray.Char in
      let tls, buf = Tls.Engine.client config in
      let rec go buf =
        Log.debug (fun m -> m "Start handshake.") ;
        Flow.send flow buf >>| reword_error flow_error >>? fun len ->
        let buf = Cstruct.shift buf len in
        if Cstruct.len buf = 0
        then return (Ok { tls = Some tls; closed = false; raw; queue; flow })
        else go buf in
      go buf

    let blit src src_off dst dst_off len =
      let dst = Cstruct.to_bigarray dst in
      Bigstringaf.blit src ~src_off dst ~dst_off ~len

    let rec recv t raw =
      Log.debug (fun m -> m "<~ Start to receive.") ;
      match Ke.N.peek t.queue with
      | [] -> (
          Log.debug (fun m -> m "<~ TLS queue is empty.") ;
          match t.tls with
          | None ->
              Log.debug (fun m -> m "<~ Connection is close.") ;
              return (Ok `End_of_flow)
          | Some tls -> (
              Log.debug (fun m -> m "<- Read the TLS flow.") ;
              Flow.recv t.flow t.raw >>| reword_error flow_error >>? function
              | `End_of_flow ->
                  Log.warn (fun m ->
                      m "<- Connection closed by underlying protocol.") ;
                  t.tls <- None ;
                  return (Ok `End_of_flow)
              | `Input 0 ->
                  Log.debug (fun m -> m "We must re-schedule, nothing to read.") ;
                  return (Ok (`Input 0))
              | `Input len ->
                  Log.debug (fun m -> m "<- Got %d byte(s)." len) ;
                  let handle raw =
                    if Tls.Engine.handshake_in_progress tls
                    then handle_handshake tls t.queue t.flow raw
                    else handle_tls tls t.queue t.flow raw in
                  Log.debug (fun m ->
                      let uid =
                        Hashtbl.hash
                          (Cstruct.to_string (Cstruct.sub t.raw 0 len)) in
                      m "<~ [%04x] Got %d bytes (handshake in progress: %b)."
                        uid len
                        (Tls.Engine.handshake_in_progress tls)) ;
                  handle (Cstruct.sub t.raw 0 len) >>? fun tls ->
                  t.tls <- tls ;
                  recv t raw))
      | _ ->
          let max = Cstruct.len raw in
          let len = min (Ke.length t.queue) max in
          Ke.N.keep_exn t.queue ~blit ~length:Cstruct.len ~off:0 ~len raw ;
          Ke.N.shift_exn t.queue len ;
          return (Ok (`Input len))

    let rec send t raw =
      Log.debug (fun m -> m "~> Start to send %d bytes." (Cstruct.len raw)) ;
      match t.tls with
      | None -> return (Error `Closed_by_peer)
      | Some tls when Tls.Engine.can_handle_appdata tls -> (
          let raw = [ raw ] in
          match Tls.Engine.send_application_data tls raw with
          | Some (tls, resp) ->
              t.tls <- Some tls ;
              flow_wr_opt t.flow (Some resp) >>? fun () ->
              return (Ok (Cstruct.lenv raw))
          | None -> return (Ok (Cstruct.lenv raw)))
      | Some tls -> (
          Flow.recv t.flow t.raw >>| reword_error flow_error >>? function
          | `End_of_flow ->
              Log.debug (fun m -> m "[-] Underlying flow already closed.") ;
              t.tls <- None ;
              return (Error `Closed_by_peer)
          | `Input 0 ->
              Log.debug (fun m -> m "[-] Underlying flow re-schedule.") ;
              return (Ok 0)
          | `Input len -> (
              let res =
                handle_handshake tls t.queue t.flow (Cstruct.sub t.raw 0 len)
              in
              res >>= function
              | Ok tls ->
                  t.tls <- tls ;
                  send t raw (* recall to finish handshake. *)
              | Error _ as err ->
                  Log.err (fun m -> m "[-] Got an error during handshake.") ;
                  return err))

    let close t =
      Log.debug (fun m -> m "!- Asking to close the TLS connection") ;
      if not t.closed
      then (
        match t.tls with
        | None ->
            Log.debug (fun m ->
                m "!- TLS state already reached EOF, close the connection.") ;
            Flow.close t.flow >>| reword_error flow_error >>= fun res ->
            Log.debug (fun m -> m "!- Underlying flow properly closed.") ;
            t.closed <- true ;
            return res
        | Some tls ->
            let _tls, resp = Tls.Engine.send_close_notify tls in
            t.tls <- None ;
            Log.debug (fun m -> m "!- Close the connection.") ;
            flow_wr_opt t.flow (Some resp) >>? fun () ->
            Flow.close t.flow >>| reword_error flow_error >>? fun () ->
            t.closed <- true ;
            return (Ok ()))
      else return (Ok ())
  end

  let protocol_with_tls :
      type edn flow.
      (edn, flow) Conduit.protocol ->
      (edn * Tls.Config.client, flow protocol_with_tls) Conduit.protocol =
   fun protocol ->
    let module Protocol = (val Conduit.impl protocol) in
    let module M = Make_protocol (Protocol) in
    Conduit.register ~protocol:(module M)

  type 'service service_with_tls = {
    service : 'service;
    tls : Tls.Config.server;
  }

  module Make_server (Service : Conduit.SERVICE) = struct
    type +'a io = 'a Conduit.io

    type configuration = Service.configuration * Tls.Config.server

    type flow = Service.flow protocol_with_tls

    type error = [ `Service of Service.error ]

    let pp_error : error Fmt.t =
     fun ppf -> function `Service err -> Service.pp_error ppf err

    let service_error err = `Service err

    type t = Service.t service_with_tls

    let init (edn, tls) =
      Service.init edn >>| reword_error service_error >>? fun service ->
      Log.info (fun m -> m "Start a TLS service.") ;
      return (Ok { service; tls })

    let accept { service; tls } =
      Service.accept service >>| reword_error service_error >>? fun flow ->
      let tls = Tls.Engine.server tls in
      let raw = Cstruct.create 0x1000 in
      let queue = Ke.create ~capacity:0x1000 Bigarray.Char in
      Log.info (fun m -> m "A TLS flow is coming.") ;
      return (Ok { tls = Some tls; closed = false; raw; queue; flow })

    let close { service; _ } =
      Service.close service >>| reword_error service_error
  end

  let service_with_tls :
      type cfg edn t flow.
      (cfg, t, flow) Conduit.Service.service ->
      (edn, flow protocol_with_tls) Conduit.protocol ->
      ( cfg * Tls.Config.server,
        t service_with_tls,
        flow protocol_with_tls )
      Conduit.Service.service =
   fun service _ ->
    let module Service = (val Conduit.Service.impl service) in
    let module M = Make_server (Service) in
    Conduit.Service.register ~service:(module M)
end
