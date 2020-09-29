(* (c) Frédéric Bour
 * (c) Romain Calascibetta *)

type ('a, 'b) refl = Refl : ('a, 'a) refl

module Tbl = struct
  (* XXX(dinosaure): [Tbl] is a small re-implementation
   * of [Hashtbl] where [find_all] is needed by [prj]. To
   * avoid an allocation of an intermediate list, we directly
   * use the underlying linked-list to do the projection.
   *
   * This implementation wants to be:
   * - deterministic (seed = 0)
   * - fast
   *
   * Memoization is done by [last_k]/[last_v] where the common use
   * of [Conduit] is a loop with multiple calls of [send]/[recv]
   * with the same [flow] value.
   *)

  type 'v t = {
    mutable size : int;
    mutable data : 'v lst array;
    mutable last_k : int;
    mutable last_v : 'v;
  }

  and 'v lst = Empty | Cons of { key : int; data : 'v; mutable next : 'v lst }

  let rec power_2_above x n =
    if x >= n
    then x
    else if x * 2 > Sys.max_array_length
    then x
    else power_2_above (x * 2) n

  let create ~epsilon size =
    let size = power_2_above 16 size in
    { size = 0; data = Array.make size Empty; last_k = 0; last_v = epsilon }

  external caml_hash : int -> int -> int -> 'a -> int = "caml_hash" [@@noalloc]

  let hash v = caml_hash 10 100 0 v

  let resize t =
    let old_data = t.data in
    let old_size = Array.length old_data in
    let new_size = old_size * 2 in
    if new_size < Sys.max_array_length
    then (
      let new_data = Array.make new_size Empty in
      let new_data_tail = Array.make new_size Empty in
      t.data <- new_data ;
      let rec insert = function
        | Empty -> ()
        | Cons { key; next; _ } as cell ->
            let new_idx = hash key land (new_size - 1) in
            (match new_data_tail.(new_idx) with
            | Empty -> new_data.(new_idx) <- cell
            | Cons tail -> tail.next <- cell) ;
            new_data_tail.(new_idx) <- cell ;
            insert next in
      for i = 0 to old_size - 1 do
        insert old_data.(i)
      done ;
      for i = 0 to new_size - 1 do
        match new_data_tail.(i) with
        | Empty -> ()
        | Cons tail -> tail.next <- Empty
      done)

  let add t key data =
    let i = hash key land (Array.length t.data - 1) in
    let v = Cons { key; data; next = t.data.(i) } in
    t.data.(i) <- v ;
    t.size <- t.size + 1 ;
    if t.size > Array.length t.data lsl 1 then resize t
end

module type S1 = sig
  type 'a t
end

module Make (Key : S1) = struct
  type t = ..

  type _ id = ..

  module type S = sig
    type x

    type t += T of x

    type _ id += Id : x id

    val witness : x Key.t
  end

  type 'a s = (module S with type x = 'a)

  type v = Value : 'a * 'a Key.t -> v

  type k = Key : 'a Key.t * ('a -> t) -> k

  let equal : type a b. a s -> b s -> (a, b) refl option =
   fun a b ->
    let module A = (val a : S with type x = a) in
    let module B = (val b : S with type x = b) in
    match A.Id with B.Id -> Some Refl | _ -> None

  let epsilon _ = raise_notrace Not_found

  let handlers = Tbl.create ~epsilon 16

  let witnesses = Hashtbl.create ~random:false 16

  module Injection (X : sig
    type t

    val witness : t Key.t
  end) : S with type x = X.t = struct
    type x = X.t

    type t += T of x

    type _ id += Id : x id

    let witness = X.witness

    let key = Key (witness, fun x -> T x)

    let value x = Value (x, witness)

    let handler = function T x -> value x | _ -> raise_notrace Not_found

    let () =
      let[@warning "-3"] uid =
        Stdlib.Obj.extension_id [%extension_constructor T] in
      Tbl.add handlers uid handler ;
      Hashtbl.add witnesses uid key
  end

  let inj (type a) (k : a Key.t) : a s =
    (module Injection (struct
      type t = a

      let witness = k
    end))

  (* XXX(dinosaure): we ensure that a value [t : t] must have an implementation
   * availble into [handlers]. By this way,
   * [let[@warning "-8"] Tbl.Cons _ = lst in] is safe where we must find an
   * implementation.
   *)

  let rec iter t uid lst =
    let[@warning "-8"] (Tbl.Cons { key = k; data = f; next = r; _ }) = lst in
    try
      if uid <> k then raise_notrace Not_found ;
      handlers.Tbl.last_v <- f ;
      f t
    with _ -> (iter [@tailcall]) t uid r

  let prj (t : t) =
    let arr = handlers.Tbl.data in
    let uid =
      Stdlib.Obj.((extension_id (extension_constructor t) [@warning "-3"]))
    in
    if handlers.Tbl.last_k == uid
    then handlers.Tbl.last_v t
    else
      let res = iter t uid arr.(Tbl.hash uid land (Array.length arr - 1)) in
      handlers.Tbl.last_k <- uid ;
      res

  let extract (t : t) (type a) ((module S) : a s) : a option =
    match t with S.T x -> Some x | _ -> None

  let bindings : unit -> k list =
   fun () -> Hashtbl.fold (fun _ v a -> v :: a) witnesses []
end
