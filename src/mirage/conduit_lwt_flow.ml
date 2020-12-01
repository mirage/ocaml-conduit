open Lwt.Infix

type flow = Conduit_mirage.flow

type error = Conduit_mirage.error

type write_error = [ Mirage_flow.write_error | Conduit_mirage.error ]

let page_size = 4096

let pp_error = Conduit_mirage.pp_error

let pp_write_error ppf = function
  | #Mirage_flow.write_error as err -> Mirage_flow.pp_write_error ppf err
  | #Conduit_mirage.error as err -> Conduit_mirage.pp_error ppf err

let read flow =
  let raw = Cstruct.create page_size in
  Conduit_mirage.recv flow raw >>= function
  | Ok `End_of_flow -> Lwt.return_ok `Eof
  | Ok (`Input len) -> Lwt.return_ok (`Data (Cstruct.sub raw 0 len))
  | Error _ as err -> Lwt.return err

let write flow raw =
  let rec go x =
    if Cstruct.len x = 0
    then Lwt.return_ok ()
    else
      Conduit_mirage.send flow x >>= function
      | Error _ as err -> Lwt.return err
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

let failwithf fmt = Format.kasprintf (fun err -> Lwt.fail (Failure err)) fmt

let close flow =
  Conduit_mirage.close flow >>= function
  | Ok () -> Lwt.return_unit
  | Error err -> failwithf "%a" Conduit_mirage.pp_error err
