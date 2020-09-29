external tick : unit -> (int64[@unboxed]) = "none" "get_tick" [@@noalloc]

type t = V : (unit -> 'a) -> t

let stabilize_garbage_collector () =
  let rec go limit last_heap_live_words =
    if limit <= 0
    then failwith "Unable to stabilize the number of live words in the heap" ;
    Gc.compact () ;
    let stat = Gc.stat () in
    if stat.Gc.live_words <> last_heap_live_words
    then go (pred limit) stat.Gc.live_words in
  go 10 0

let runnable f i =
  for _ = 1 to i do
    ignore @@ Sys.opaque_identity (f ())
  done
  [@@inline]

let samples = 1000

let run t =
  let idx = ref 0 in
  let run = ref 0 in
  let (V fn) = t in

  let m = Array.create_float (samples * 2) in

  stabilize_garbage_collector () ;

  while !idx < samples do
    let current_run = !run in
    let current_idx = !idx in

    let time_0 = tick () in

    runnable fn current_run ;

    let time_1 = tick () in

    m.((current_idx * 2) + 0) <- float_of_int current_run ;
    m.((current_idx * 2) + 1) <- Int64.to_float (Int64.sub time_1 time_0) ;

    let next =
      (max : int -> int -> int)
        (int_of_float (float_of_int current_run *. 1.01))
        (succ current_run) in
    run := next ;
    incr idx
  done ;

  Array.init samples (fun i -> [| m.((i * 2) + 0); m.((i * 2) + 1) |])
