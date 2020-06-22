external tick : unit -> (int64[@unboxed]) = "none" "get_tick" [@@noalloc]

module None = struct
  type +'a t = 'a

  let bind x f = f x

  let return x = x
end

module Tuyau = Conduit.Make (None) (Bytes) (String)

let t1 = ref 0L

module Fake_protocol0 = struct
  type input = bytes

  and output = string

  and +'a s = 'a

  type endpoint = Unix.file_descr

  type flow = Unix.file_descr

  type error = |

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .

  let connect x = Ok x

  let recv _ _ = Ok `End_of_flow

  let send fd v =
    t1 := tick () ;
    let _ = Unix.write_substring fd v 0 (String.length v) in
    Ok 0

  let close _ = Ok ()
end

module Fake_protocol1 = struct
  type input = bytes

  and output = string

  and +'a s = 'a

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

  and +'a s = 'a

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

let fully_abstr () =
  let open Rresult in
  Tuyau.connect Unix.stderr fake0 >>= fun flow ->
  let t0 = tick () in
  Tuyau.send flow hello_world >>= fun _len ->
  let t3 = Int64.sub (tick ()) !t1 in
  let t2 = tick () in
  Tuyau.send flow hello_world >>= fun _len ->
  R.ok (Int64.sub !t1 t0, t3, Int64.sub !t1 t2)

let abstr () =
  let open Rresult in
  let module Protocol = (val Tuyau.impl fake0) in
  Tuyau.connect Unix.stderr fake0 >>= fun flow ->
  let (Tuyau.Flow (flow, (module Flow))) = Tuyau.flow flow in
  let t0 = tick () in
  Flow.send flow hello_world |> R.reword_error (R.msgf "%a" Flow.pp_error)
  >>= fun _len ->
  let t3 = Int64.sub (tick ()) !t1 in
  let t2 = tick () in
  Flow.send flow hello_world |> R.reword_error (R.msgf "%a" Flow.pp_error)
  >>= fun _len -> R.ok (Int64.sub !t1 t0, t3, Int64.sub !t1 t2)

let concrete () =
  let t0 = tick () in
  let _ =
    Unix.write_substring Unix.stderr hello_world 0 (String.length hello_world)
  in
  let t1 = tick () in
  Ok (Int64.sub t1 t0)

let () =
  let _ =
    Unix.write_substring Unix.stderr hello_world 0 (String.length hello_world)
  in
  let[@warning "-8"] (Ok ts) = concrete () in
  t1 := 0L ;
  let[@warning "-8"] (Ok (ts0, ts1, ts2)) = fully_abstr () in
  t1 := 0L ;
  let[@warning "-8"] (Ok (tsa, tsb, tsc)) = abstr () in
  t1 := 0L ;
  Fmt.pr "fully-abstr:\t%Ldns, %Ldns, %Ldns.\n%!" ts0 ts1 ts2 ;
  Fmt.pr "abstr:\t\t%Ldns, %Ldns, %Ldns.\n%!" tsa tsb tsc ;
  Fmt.pr "concrete:\t%Ldns.\n%!" ts
