external tick : unit -> (int64[@unboxed]) = "none" "get_tick" [@@noalloc]

module None = struct
  type +'a t = 'a

  let bind x f = f x

  let return x = x
end

module Tuyau = Conduit.Make (None) (Bytes) (String)

module Fake_protocol0 = struct
  type input = bytes

  and output = string

  and +'a io = 'a

  type endpoint = Unix.file_descr

  type flow = Unix.file_descr

  type error = |

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .

  let connect x = Ok x

  let recv _ _ = Ok `End_of_flow

  let send _ _ =
    for _ = 0 to 500 do
      ()
    done ;
    Ok 0

  let close _ = Ok ()
end

module Fake_protocol1 = struct
  type input = bytes

  and output = string

  and +'a io = 'a

  type endpoint = Unix.file_descr

  type flow = Unix.file_descr

  type error = |

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .

  let connect x = Ok x

  let recv _ _ = Ok `End_of_flow

  let send _ _ = assert false

  let close _ = Ok ()
end

module Fake_protocol2 = struct
  type input = bytes

  and output = string

  and +'a io = 'a

  type endpoint = Unix.file_descr

  type flow = Unix.file_descr

  type error = |

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .

  let connect x = Ok x

  let recv _ _ = Ok `End_of_flow

  let send _ _ = assert false

  let close _ = Ok ()
end

let fake0 = Tuyau.register (module Fake_protocol0)

let fake1 = Tuyau.register (module Fake_protocol1)

let fake2 = Tuyau.register (module Fake_protocol2)

module R0 = (val Tuyau.repr fake0)

module R1 = (val Tuyau.repr fake1)

module R2 = (val Tuyau.repr fake2)

let hello_world = "Hello World!\n"

let fn_fully_abstr flow = Benchmark.V (fun () -> Tuyau.send flow hello_world)

let fn_abstr = function
  | R0.T flow -> Benchmark.V (fun () -> Fake_protocol0.send flow hello_world)
  | R1.T flow -> Benchmark.V (fun () -> Fake_protocol1.send flow hello_world)
  | R2.T flow -> Benchmark.V (fun () -> Fake_protocol2.send flow hello_world)
  | _ -> assert false

type result = {
  with_conduit : float;
  with_conduit_r2 : float;
  without_conduit : float;
  without_conduit_r2 : float;
}
[@@deriving to_yojson]

let print_json est0 est1 r0 r1 =
  let with_conduit = est0.(0) in
  let with_conduit_r2 = r0 in
  let without_conduit = est1.(0) in
  let without_conduit_r2 = r1 in
  let res =
    { with_conduit; with_conduit_r2; without_conduit; without_conduit_r2 } in
  let fmt = stdout |> Format.formatter_of_out_channel in
  let open Yojson.Safe in
  let obj =
    `Assoc
      [
        ( "results",
          `Assoc
            [
              ("name", `String "benchmarks"); ("metrics", result_to_yojson res);
            ] );
      ] in
  pretty_print fmt obj

let print_stdout est0 est1 r0 r1 =
  Fmt.pr "with Conduit:\t\t%fns (r²: %f).\n%!" est0.(0) r0 ;
  Fmt.pr "without Conduit:\t%fns (r²: %f).\n%!" est1.(0) r1 ;
  if r0 >= 0.99 && r1 >= 0.99
  then Fmt.pr "Overhead:\t\t%fns.\n%!" (est0.(0) -. est1.(0))
  else Fmt.epr "Bad regression coefficients!\n%!"

let run json =
  let open Rresult in
  Tuyau.connect fake0 Unix.stderr >>= fun flow ->
  Tuyau.send flow hello_world >>= fun _ ->
  let samples0 = Benchmark.run (fn_fully_abstr flow) in
  let samples1 = Benchmark.run (fn_abstr flow) in

  match
    ( Linear_algebra.ols (fun m -> m.(1)) [| (fun m -> m.(0)) |] samples0,
      Linear_algebra.ols (fun m -> m.(1)) [| (fun m -> m.(0)) |] samples1 )
  with
  | Ok (estimate0, r0), Ok (estimate1, r1) ->
      (match json with
      | true -> print_json estimate0 estimate1 r0 r1
      | false -> print_stdout estimate0 estimate1 r0 r1) ;
      Ok ()
  | Error err, _ -> Error err
  | _, Error err -> Error err

open Cmdliner

let json = Arg.(value & flag & info [ "j"; "json" ])

let main json =
  match run json with
  | Ok v -> v
  | Error (`Msg err) -> Fmt.epr "%s: %s.\n%!" Sys.argv.(0) err
  | Error (`Not_found _edn) -> assert false

let cmd = (Term.(const main $ json), Term.info "run benchmarks")

let () = Term.(exit @@ eval cmd)
