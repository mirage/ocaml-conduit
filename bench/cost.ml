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

let fake0 = Tuyau.register ~protocol:(module Fake_protocol0)

let fake1 = Tuyau.register ~protocol:(module Fake_protocol1)

let fake2 = Tuyau.register ~protocol:(module Fake_protocol2)

let hello_world = "Hello World!\n"

let fn_fully_abstr flow = Benchmark.V (fun () -> Tuyau.send flow hello_world)

let fn_abstr (Tuyau.Flow (flow, (module Flow))) =
  Benchmark.V (fun () -> Flow.send flow hello_world)

let run () =
  let open Rresult in
  Tuyau.connect Unix.stderr fake0 >>= fun flow ->
  Tuyau.send flow hello_world >>= fun _ ->
  let samples0 = Benchmark.run (fn_fully_abstr flow) in
  let samples1 = Benchmark.run (fn_abstr (Tuyau.unpack flow)) in

  match
    ( Linear_algebra.ols (fun m -> m.(1)) [| (fun m -> m.(0)) |] samples0,
      Linear_algebra.ols (fun m -> m.(1)) [| (fun m -> m.(0)) |] samples1 )
  with
  | Ok (estimate0, r0), Ok (estimate1, r1) ->
      Fmt.pr "with Conduit:\t\t%fns (r²: %f).\n%!" estimate0.(0) r0 ;
      Fmt.pr "without Conduit:\t%fns (r²: %f).\n%!" estimate1.(0) r1 ;
      if r0 >= 0.99 && r1 >= 0.99
      then Fmt.pr "Overhead:\t\t%fns.\n%!" (estimate0.(0) -. estimate1.(0))
      else Fmt.epr "Bad regression coefficients!\n%!" ;
      Ok ()
  | Error err, _ -> Error err
  | _, Error err -> Error err

let () =
  match run () with
  | Ok v -> v
  | Error (`Msg err) -> Fmt.epr "%s: %s.\n%!" Sys.argv.(0) err
  | Error `Not_found -> assert false
