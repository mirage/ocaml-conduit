module Unix_scheduler = struct
  type +'a t = 'a

  let bind x f = f x

  let return x = x
end

module Conduit = Conduit.Make (Unix_scheduler) (Bytes) (String)

let recv =
  let pp ppf = function
    | `Input len -> Fmt.pf ppf "@[<1>(`Input@ %d)@]" len
    | `End_of_flow -> Fmt.string ppf "`End_of_flow" in
  let equal a b =
    match (a, b) with
    | `Input a, `Input b -> a = b
    | `End_of_flow, `End_of_flow -> true
    | _ -> false in
  Alcotest.testable pp equal

let send = Alcotest.int

let error =
  let pp ppf = function
    | #Rresult.R.msg as v -> Rresult.R.pp_msg ppf v
    | `Not_found -> Fmt.string ppf "`Not_found" in
  let equal a b =
    match (a, b) with
    | `Msg a, `Msg b -> a = b
    | `Not_found, `Not_found -> true
    | _ -> false in
  Alcotest.testable pp equal

module Memory_flow0 = struct
  type input = bytes

  and output = string

  type +'a io = 'a

  type flow = {
    mutable i : string;
    o : bytes;
    mutable p : int;
    mutable c : bool;
  }

  type endpoint = string * bytes

  type error = [ `Closed ]

  let closed_by_peer = "Closed by peer"

  let pp_error ppf = function `Closed -> Fmt.string ppf closed_by_peer

  let connect (i, o) = Ok { i; o; p = 0; c = false }

  let recv flow buf =
    let len = min (String.length flow.i) (Bytes.length buf) in
    if len = 0
    then (
      flow.c <- true ;
      Ok `End_of_flow)
    else (
      Bytes.blit_string flow.i 0 buf 0 len ;
      flow.i <- String.sub flow.i len (String.length flow.i - len) ;
      Ok (`Input len))

  let send flow str =
    if flow.c
    then Error `Closed
    else
      let len = min (Bytes.length flow.o - flow.p) (String.length str) in
      Bytes.blit_string str 0 flow.o flow.p len ;
      flow.p <- flow.p + len ;
      Ok len

  let close flow =
    flow.c <- true ;
    Ok ()
end

let memory0 = Conduit.register ~protocol:(module Memory_flow0)

let test_input_string =
  Alcotest.test_case "input string" `Quick @@ fun () ->
  let open Rresult in
  let flow = Conduit.connect ("Hello World!", Bytes.empty) memory0 in
  Alcotest.(check bool) "connect" (R.is_ok flow) true ;
  let flow = R.get_ok flow in
  let buf0 = Bytes.create 12 in
  let buf1 = Bytes.create 12 in
  let res0 = Conduit.recv flow buf0 in
  let res1 = Conduit.recv flow buf1 in
  let res2 = Conduit.send flow "Hello World!" in
  Alcotest.(check (result recv error)) "res0" res0 (Ok (`Input 12)) ;
  Alcotest.(check string) "buf0" (Bytes.to_string buf0) "Hello World!" ;
  Alcotest.(check (result recv error)) "res1" res1 (Ok `End_of_flow) ;
  Alcotest.(check (result send error))
    "res2" res2
    (Error (`Msg Memory_flow0.closed_by_peer))

let test_output_string =
  Alcotest.test_case "output string" `Quick @@ fun () ->
  let open Rresult in
  let buf = Bytes.create 12 in
  let flow = Conduit.connect ("", buf) memory0 in
  Alcotest.(check bool) "connect" (R.is_ok flow) true ;
  let flow = R.get_ok flow in
  let res0 = Conduit.send flow "Hell" in
  let res1 = Conduit.send flow "o Wo" in
  let res2 = Conduit.send flow "rld!" in
  let res3 = Conduit.send flow "?!?!" in
  let res4 = Conduit.recv flow Bytes.empty in
  Alcotest.(check (result send error)) "res0" res0 (Ok 4) ;
  Alcotest.(check (result send error)) "res1" res1 (Ok 4) ;
  Alcotest.(check (result send error)) "res2" res2 (Ok 4) ;
  Alcotest.(check (result send error)) "res3" res3 (Ok 0) ;
  Alcotest.(check (result recv error)) "res4" res4 (Ok `End_of_flow) ;
  Alcotest.(check string) "buf" (Bytes.to_string buf) "Hello World!"

module Memory_flow1 = struct
  type input = bytes

  and output = string

  type +'a io = 'a

  type flow = {
    mutable i : string list;
    o : bytes list;
    mutable p : int;
    mutable c : bool;
  }

  type endpoint = string list * bytes list

  type error = [ `Closed ]

  let closed_by_peer = "Closed by peer"

  let pp_error ppf = function `Closed -> Fmt.string ppf closed_by_peer

  let connect (i, o) = Ok { i; o; p = 0; c = false }

  let rec shift n = function
    | [] -> []
    | x :: r ->
        if String.length x <= n
        then shift (n - String.length x) r
        else String.sub x n (String.length x - n) :: r

  let recv flow buf =
    let max = Bytes.length buf in
    let acc = ref 0 in
    List.iter
      (fun x ->
        if !acc < max
        then (
          let len = min (max - !acc) (String.length x) in
          Bytes.blit_string x 0 buf !acc len ;
          acc := !acc + len))
      flow.i ;
    flow.i <- shift !acc flow.i ;
    if !acc = 0
    then (
      flow.c <- true ;
      Ok `End_of_flow)
    else Ok (`Input !acc)

  let ( <.> ) f g x = f (g x)

  let send flow str =
    if flow.c
    then Error `Closed
    else
      let top = String.length str in
      let pos = ref flow.p in
      let acc = ref 0 in
      List.iter
        (fun x ->
          if !acc < top && !pos - Bytes.length x < 0
          then (
            let len = max 0 (Bytes.length x + (!pos + !acc)) in
            let len = min (top - !acc) len in
            Bytes.blit_string str !acc x (!pos + !acc) len ;
            acc := !acc + len) ;
          pos := !pos - Bytes.length x)
        flow.o ;
      flow.p <- flow.p + !acc ;
      if flow.p = List.fold_right (( + ) <.> Bytes.length) flow.o 0
      then flow.c <- true ;
      Ok !acc

  let close flow =
    flow.c <- true ;
    Ok ()
end

let memory1 = Conduit.register ~protocol:(module Memory_flow1)

let test_input_strings =
  Alcotest.test_case "input strings" `Quick @@ fun () ->
  let open Rresult in
  let flow =
    Conduit.connect ([ ""; "123"; "45"; "6789"; "0" ], [ Bytes.empty ]) memory1
  in
  Alcotest.(check bool) "connect" (R.is_ok flow) true ;
  let flow = R.get_ok flow in
  let buf0 = Bytes.create 5 in
  let buf1 = Bytes.create 5 in
  let res0 = Conduit.recv flow buf0 in
  let res1 = Conduit.recv flow buf1 in
  let res2 = Conduit.recv flow Bytes.empty in
  let res3 = Conduit.recv flow Bytes.empty in
  let res4 = Conduit.send flow "" in
  Alcotest.(check (result recv error)) "res0" res0 (Ok (`Input 5)) ;
  Alcotest.(check (result recv error)) "res1" res1 (Ok (`Input 5)) ;
  Alcotest.(check string) "buf0" (Bytes.to_string buf0) "12345" ;
  Alcotest.(check string) "buf1" (Bytes.to_string buf1) "67890" ;
  Alcotest.(check (result recv error)) "res2" res2 (Ok `End_of_flow) ;
  Alcotest.(check (result recv error)) "res3" res3 (Ok `End_of_flow) ;
  Alcotest.(check (result send error))
    "res4" res4
    (Error (`Msg Memory_flow1.closed_by_peer))

let test_output_strings =
  Alcotest.test_case "output strings" `Quick @@ fun () ->
  let open Rresult in
  let bufs = [ Bytes.create 4; Bytes.empty; Bytes.create 2; Bytes.create 6 ] in
  let flow = Conduit.connect ([], bufs) memory1 in
  Alcotest.(check bool) "connect" (R.is_ok flow) true ;
  let flow = R.get_ok flow in
  let res0 = Conduit.send flow "Hello" in
  let res1 = Conduit.send flow " " in
  let res2 = Conduit.send flow "World!" in
  let res3 = Conduit.send flow "?!?!" in
  Alcotest.(check (result send error)) "res0" res0 (Ok 5) ;
  Alcotest.(check (result send error)) "res1" res1 (Ok 1) ;
  Alcotest.(check (result send error)) "res2" res2 (Ok 6) ;
  Alcotest.(check (result send error))
    "res3" res3
    (Error (`Msg Memory_flow1.closed_by_peer)) ;
  Alcotest.(check string)
    "bufs"
    (String.concat "" (List.map Bytes.to_string bufs))
    "Hello World!"

let () =
  Alcotest.run "flow"
    [
      ( "memory",
        [
          test_input_string;
          test_output_string;
          test_input_strings;
          test_output_strings;
        ] );
    ]
