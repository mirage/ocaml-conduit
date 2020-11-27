open Lwt.Infix

type flow = Conduit_mirage.flow

type error = Conduit_mirage.error

type write_error = [ Mirage_flow.write_error | `Conduit of Conduit_mirage.error ]

let pp_error = Conduit_mirage.pp_error

let pp_write_error ppf = function
  | #Mirage_flow.write_error as err -> Mirage_flow.pp_write_error ppf err
  | `Conduit err -> Conduit_mirage.pp_error ppf err

let read flow =
  let raw = Cstruct.create 0x1000 in
  Conduit_mirage.recv flow raw >|= function
  | Ok `End_of_flow -> Ok `Eof
  | Ok (`Input len) -> Ok (`Data (Cstruct.sub raw 0 len))
  | Error _ as err -> err

let write flow raw =
  let rec go x =
    if Cstruct.len x = 0
    then Lwt.return_ok ()
    else
      Conduit_mirage.send flow x >>= function
      | Error err -> Lwt.return (Error (`Conduit err))
      | Ok len -> go (Cstruct.shift x len) in
  go raw

let writev flow cs =
  let rec go = function
    | [] -> Lwt.return_ok ()
    | x :: r -> (
        write flow x >>= function
        | Ok () -> go r
        | Error _ as err -> Lwt.return err) in
  go cs

let close flow = Conduit_mirage.close flow >>= fun _ -> Lwt.return_unit
