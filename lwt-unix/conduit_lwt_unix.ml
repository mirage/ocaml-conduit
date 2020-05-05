include Conduit_lwt

let failf fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

let io_of_flow flow =
  let open Lwt.Infix in
  let ic_closed = ref false and oc_closed = ref false in
  let close () =
    if !ic_closed && !oc_closed
    then
      close flow >>= function
      | Ok () -> Lwt.return_unit
      | Error err -> failf "%a" pp_error err
    else Lwt.return_unit in
  let ic_close () =
    ic_closed := true ;
    close () in
  let oc_close () =
    oc_closed := true ;
    close () in
  let recv buf off len =
    let raw = Cstruct.of_bigarray buf ~off ~len in
    recv flow raw >>= function
    | Ok (`Input len) -> Lwt.return len
    | Ok `End_of_input -> Lwt.return 0
    | Error err -> failf "%a" pp_error err in
  let ic = Lwt_io.make ~close:ic_close ~mode:Lwt_io.input recv in
  let send buf off len =
    let raw = Cstruct.of_bigarray buf ~off ~len in
    send flow raw >>= function
    | Ok len -> Lwt.return len
    | Error err -> failf "%a" pp_error err in
  let oc = Lwt_io.make ~close:oc_close ~mode:Lwt_io.output send in
  (ic, oc)
