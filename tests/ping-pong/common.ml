module type S = sig
  include Conduit.S

  type 'a condition

  val serve :
    handler:('flow -> unit io) ->
    service:('cfg, 'master, 'flow) Service.service ->
    'cfg ->
    unit condition * unit io
end

module type CONDITION = sig
  type 'a t
end

let ( <.> ) f g x = f (g x)

module Make
    (IO : Conduit.IO)
    (Condition : CONDITION)
    (Conduit : S
                 with type +'a io = 'a IO.t
                  and type 'a condition = 'a Condition.t
                  and type input = Cstruct.t
                  and type output = Cstruct.t) =
struct
  let return = IO.return

  let ( >>= ) = IO.bind

  let ( >>? ) x f =
    x >>= function Ok x -> f x | Error err -> IO.return (Error err)

  let localhost = Domain_name.(host_exn <.> of_string_exn) "localhost"

  (* Server part *)

  let getline queue =
    let exists ~predicate queue =
      let pos = ref 0 and res = ref (-1) in
      Ke.Rke.iter
        (fun chr ->
          if predicate chr then res := !pos ;
          incr pos)
        queue ;
      if !res = -1 then None else Some !res in
    let blit src src_off dst dst_off len =
      Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len in
    match exists ~predicate:(( = ) '\n') queue with
    | Some pos ->
        let tmp = Bytes.create pos in
        Ke.Rke.N.keep_exn queue ~blit ~length:Bytes.length ~off:0 ~len:pos tmp ;
        Ke.Rke.N.shift_exn queue (pos + 1) ;
        Some (Bytes.unsafe_to_string tmp)
    | None -> None

  let getline queue flow =
    let tmp = Cstruct.create 0x1000 in
    let blit src src_off dst dst_off len =
      let src = Cstruct.to_bigarray src in
      Bigstringaf.blit src ~src_off dst ~dst_off ~len in
    let rec go () =
      match getline queue with
      | Some line -> IO.return (Ok (`Line line))
      | None -> (
          Conduit.recv flow tmp >>? function
          | `End_of_flow -> IO.return (Ok `Close)
          | `Input len ->
              Ke.Rke.N.push queue ~blit ~length:Cstruct.len ~off:0 ~len tmp ;
              go ()) in
    go ()

  let pong = Cstruct.of_string "pong\n"

  let ping = Cstruct.of_string "ping\n"

  let transmission flow =
    let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
    let rec go () =
      getline queue flow >>= function
      | Ok `Close | Error _ -> Conduit.close flow
      | Ok (`Line "ping") ->
          Fmt.epr "[!] received ping.\n%!" ;
          Conduit.send flow pong >>? fun _ -> go ()
      | Ok (`Line "pong") ->
          Fmt.epr "[!] received pong.\n%!" ;
          Conduit.send flow ping >>? fun _ -> go ()
      | Ok (`Line line) ->
          Fmt.epr "[!] received %S.\n%!" line ;
          Conduit.send flow (Cstruct.of_string (line ^ "\n")) >>? fun _ ->
          Conduit.close flow in
    go () >>= function
    | Error err -> Fmt.failwith "%a" Conduit.pp_error err
    | Ok () -> return ()

  let server :
      type cfg service flow.
      cfg ->
      protocol:(_, flow) Conduit.protocol ->
      service:(cfg, service, flow) Conduit.Service.service ->
      unit Condition.t * unit IO.t =
   fun cfg ~protocol ~service ->
    Conduit.serve
      ~handler:(fun flow -> transmission (Conduit.pack protocol flow))
      ~service cfg

  (* part *)

  let client ~resolvers domain_name responses =
    Conduit.resolve resolvers domain_name >>? fun flow ->
    let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
    let rec go = function
      | [] -> Conduit.close flow
      | line :: rest -> (
          Conduit.send flow (Cstruct.of_string (line ^ "\n")) >>? fun _ ->
          getline queue flow >>? function
          | `Close -> Conduit.close flow
          | `Line "pong" -> go rest
          | `Line _ -> Conduit.close flow) in
    go responses

  let client ~resolvers filename =
    let rec go acc ic =
      match input_line ic with
      | line -> go (line :: acc) ic
      | exception End_of_file -> List.rev acc in
    let ic = open_in filename in
    let responses = go [] ic in
    close_in ic ;
    client ~resolvers localhost responses >>= function
    | Ok () -> IO.return ()
    | Error `Closed_by_peer -> IO.return ()
    | Error (#Conduit.error as err) ->
        Fmt.epr "client: %a.\n%!" Conduit.pp_error err ;
        IO.return ()
end
