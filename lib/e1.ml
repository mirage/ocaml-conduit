(* (c) Daniel Bünzli *)

module Refl = struct
  type ('a, 'b) t = Refl : ('a, 'a) t
end

module Type = struct
  type 'a t = ..
end

module type TYPE = sig
  type t

  type _ Type.t += T : t Type.t
end

type 'a t = (module TYPE with type t = 'a)

let tid () (type x) =
  let module X = struct
    type t = x

    type _ Type.t += T : t Type.t
  end in
  (module X : TYPE with type t = x)

let eq : type a b. a t -> b t -> (a, b) Refl.t option =
 fun a b ->
  let module A = (val a : TYPE with type t = a) in
  let module B = (val b : TYPE with type t = b) in
  match A.T with B.T -> Some Refl.Refl | _ -> None

type identifier = int

let identifier_equal a b = (compare : int -> int -> int) a b = 0

let identifier_compare a b = (compare : int -> int -> int) a b

module Make (K : Sigs.FUNCTOR) (V : Sigs.FUNCTOR) = struct
  module Key = struct
    type 'a info = 'a K.t

    type 'a key = { uid : identifier; tid : 'a t; info : 'a K.t }

    let uid =
      let x = ref (-1) in
      fun () ->
        incr x ;
        !x

    let create info =
      let uid = uid () in
      let tid = tid () in
      { uid; tid; info }

    let info { info; _ } = info

    let identifier { uid; _ } = uid

    type t = K : 'a key -> t

    let hide k = K k

    let equal (K a) (K b) = (compare : int -> int -> int) a.uid b.uid = 0

    let compare (K a) (K b) = (compare : int -> int -> int) a.uid b.uid

    let ( == ) : type a b. a key -> b key -> (a, b) Refl.t option =
     fun a b -> eq a.tid b.tid
  end

  type 'a key = 'a Key.key

  module Map = Map.Make (Key)

  type binding = B : 'a key * 'a V.t -> binding

  type t = binding Map.t

  let empty = Map.empty

  let is_empty = Map.is_empty

  let mem k m = Map.mem (Key.K k) m

  let add k v m = Map.add (Key.K k) (B (k, v)) m

  let singleton k v = Map.singleton (Key.K k) (B (k, v))

  let rem k m = Map.remove (Key.K k) m

  let len m = Map.cardinal m

  let find : type a. a key -> t -> a V.t option =
   fun k m ->
    match Map.find (K k) m with
    | B (k', v) -> (
        match eq k.Key.tid k'.Key.tid with
        | Some Refl.Refl -> Some v
        | None -> None)
    | exception Not_found -> None

  type v = Value : 'a key * 'a V.t -> v

  let bindings m =
    Map.bindings m
    |> List.fold_left
         (fun a (Key.K k, B (k', v)) ->
           match eq k.Key.tid k'.Key.tid with
           | Some Refl.Refl -> Value (k, v) :: a
           | None -> a)
         []
end
