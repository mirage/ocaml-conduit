module Unix_scheduler = struct
  type +'a t = 'a

  let bind x f = f x

  let return x = x
end

module Conduit = Conduit.Make (Unix_scheduler) (Bytes) (String)

module Dummy (Edn : sig
  type t
end) =
struct
  type input = bytes

  and output = string

  type +'a io = 'a

  type endpoint = Edn.t

  type flow = unit

  type error = |

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .

  let connect _ = Ok ()

  let recv _ _ = Ok `End_of_flow

  let send _ _ = Ok 0

  let close _ = Ok ()
end

module Dummy_int = Dummy (struct
  type t = int
end)

module Dummy_string = Dummy (struct
  type t = string
end)

module Dummy_unit = Dummy (struct
  type t = unit
end)

let dummy_int = Conduit.register ~protocol:(module Dummy_int)

let dummy_string = Conduit.register ~protocol:(module Dummy_string)

let dummy_unit = Conduit.register ~protocol:(module Dummy_unit)

let ( <.> ) f g x = f (g x)

let localhost = Domain_name.(host_exn <.> of_string_exn) "localhost"

let all_resolvers =
  Alcotest.test_case "all resolvers" `Quick @@ fun () ->
  let int_called = ref true in
  let int _ = Some 0 in

  let string_called = ref true in
  let string _ = Some "Hello World!" in

  let unit_called = ref true in
  let unit _ = Some () in

  let resolvers =
    Conduit.empty
    |> Conduit.add dummy_int int
    |> Conduit.add dummy_string string
    |> Conduit.add dummy_unit unit in
  let _ = Conduit.resolve resolvers localhost in
  Alcotest.(check bool) "call int" !int_called true ;
  Alcotest.(check bool) "call string" !string_called true ;
  Alcotest.(check bool) "call unit" !unit_called true

let priorities =
  Alcotest.test_case "priorities" `Quick @@ fun () ->
  let count = ref 0 in

  let int_called = ref None in
  let int _ =
    int_called := Some !count ;
    incr count ;
    Some 0 in

  let string_called = ref None in
  let string _ =
    string_called := Some !count ;
    incr count ;
    Some "Hello World!" in

  let unit_called = ref None in
  let unit _ =
    unit_called := Some !count ;
    incr count ;
    Some () in

  let resolvers =
    Conduit.empty
    |> Conduit.add ~priority:0 dummy_int int
    |> Conduit.add ~priority:10 dummy_string string
    |> Conduit.add dummy_unit unit in
  let _ = Conduit.resolve resolvers localhost in
  Alcotest.(check (option int)) "call int" !int_called (Some 0) ;
  Alcotest.(check (option int)) "call string" !string_called (Some 1) ;
  Alcotest.(check (option int)) "call unit" !unit_called (Some 2) ;

  int_called := None ;
  string_called := None ;
  unit_called := None ;
  count := 0 ;

  let resolvers =
    Conduit.empty
    |> Conduit.add dummy_int int
    |> Conduit.add ~priority:0 dummy_string string
    |> Conduit.add dummy_unit unit in
  let _ = Conduit.resolve resolvers localhost in
  Alcotest.(check (option int)) "call int" !int_called (Some 2) ;
  Alcotest.(check (option int)) "call string" !string_called (Some 0) ;
  Alcotest.(check (option int)) "call unit" !unit_called (Some 1) ;

  int_called := None ;
  string_called := None ;
  unit_called := None ;
  count := 0 ;

  let resolvers =
    Conduit.empty
    |> Conduit.add dummy_int int
    |> Conduit.add dummy_string string
    |> Conduit.add dummy_unit unit in
  let _ = Conduit.resolve resolvers localhost in
  Alcotest.(check (option int)) "call int" !int_called (Some 2) ;
  Alcotest.(check (option int)) "call string" !string_called (Some 1) ;
  Alcotest.(check (option int)) "call unit" !unit_called (Some 0)

let only_one =
  Alcotest.test_case "only one" `Quick @@ fun () ->
  let int_called = ref true in
  let int _ = Some 0 in

  let string_called = ref true in
  let string _ = Some "Hello World!" in

  let unit_called = ref true in
  let unit _ = Some () in

  let resolvers =
    Conduit.empty
    |> Conduit.add dummy_int int
    |> Conduit.add dummy_string string
    |> Conduit.add dummy_unit unit in
  let _ = Conduit.resolve resolvers ~protocol:dummy_string localhost in
  Alcotest.(check bool) "call int" !int_called true ;
  Alcotest.(check bool) "call string" !string_called true ;
  Alcotest.(check bool) "call unit" !unit_called true

let () =
  Alcotest.run "resolvers"
    [ ("resolve", [ all_resolvers; priorities; only_one ]) ]
