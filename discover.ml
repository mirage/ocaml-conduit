open Printf

let () = Printexc.record_backtrace true

let verbose =
  try
    ignore (Sys.getenv "CONDUIT_DISCOVER_VERBOSE");
    true
  with Not_found -> false

let write_lines ~file ~lines =
  let out = open_out file in
  lines |> List.iter (fun m -> output_string out (m ^ "\n"));
  close_out out

module Ocb = struct
  let tags_of_packages pkgs =
    pkgs
    |> List.map (sprintf "package(%s)")
    |> String.concat ","

  let lib_targets name =
    [ "cma" ; "cmxa" ; "cmxs" ]
    |> List.map (fun ext -> sprintf "lib/%s.%s" name ext)

  let bin_targets name =
    [sprintf "tests/%s.native" (String.lowercase name)]
end

module Flag = struct
  type t =
    { name: string
    ; findlib_checks: string list }

  let check flag =
    let pkgs = String.concat " " flag.findlib_checks in
    let null = if Sys.os_type = "Win32" then "NUL" else "/dev/null" in
    let cmd = sprintf "ocamlfind query %s 1>%s 2>%s" pkgs null null in
    let res = Sys.command cmd = 0 in
    if verbose then
      printf "Checking %s(findlib: %s): %s\n%!"
        flag.name pkgs (if res then "yes" else "no");
    res

  let flags = ref []

  let mk suffix findlib_checks =
    let name = "HAVE_" ^ (String.uppercase suffix) in
    let f = { name ; findlib_checks } in
    flags :=  f :: !flags;
    f

  let flags_on = lazy (List.filter check !flags)

  let has_flag f = List.mem f (Lazy.force flags_on)
  let has_flags = List.for_all has_flag
  let subset_flags = List.filter has_flag

  let mlh_config =
    lazy (
      !flags
      |> List.map (fun f -> sprintf "#let %s = %b" f.name (has_flag f))
    )

  let uniques xs = List.sort_uniq compare xs
end

module Pkg = struct
  type t =
    { name: string
    ; findlib: string list
    ; need_flags: Flag.t list
    ; opt_flags: Flag.t list
    ; always_modules: string list
    ; flag_modules: (string * Flag.t) list }

  let empty =
    { name = ""
    ; need_flags = []
    ; opt_flags = []
    ; always_modules = []
    ; findlib = []
    ; flag_modules = [] }
end

let async       = Flag.mk "async" ["async"]
let async_ssl   = Flag.mk "async_ssl" ["async_ssl"]
let lwt         = Flag.mk "lwt" ["lwt"]
let lwt_ssl     = Flag.mk "lwt_ssl" ["lwt.ssl"]
let lwt_tls     = Flag.mk "lwt_tls" ["tls.lwt"]
let mirage      = Flag.mk "mirage" ["mirage-types-lwt"; "mirage-flow-lwt"; "dns.mirage"]
let mirage_tls  = Flag.mk "mirage_tls" ["tls"; "tls.mirage"]
let vchan       = Flag.mk "vchan" ["vchan"]
let vchan_lwt   = Flag.mk "vchan_lwt" ["vchan.lwt"]
let launchd_lwt = Flag.mk "launchd_lwt" ["launchd.lwt"]

let base_findlib =
  ["sexplib" ; "ipaddr" ; "cstruct" ; "uri" ; "stringext" ; "logs"]

module Libs = struct
  open Pkg

  let conduit =
    { empty with
      name = "conduit"
    ; always_modules = [ "Conduit" ; "Conduit_trie" ; "Resolver" ] }

  let conduit_async =
    { empty with
      name = "conduit-async"
    ; findlib = ["ipaddr.unix"]
    ; need_flags = [ async ]
    ; always_modules = [ "Conduit_async" ]
    ; flag_modules = ["Conduit_async_ssl", async_ssl] }

  let conduit_lwt =
    { empty with
      name = "conduit-lwt"
    ; need_flags = [ lwt ]
    ; always_modules = [ "Resolver_lwt" ] }

  let conduit_lwt_unix =
    { name = "conduit-lwt-unix"
    ; findlib = [ "lwt.unix" ; "ipaddr.unix" ; "uri.services"]
    ; need_flags = [ lwt ]
    ; always_modules = [ "Conduit_lwt_server"
                       ; "Conduit_lwt_unix"
                       ; "Resolver_lwt_unix" ]
    ; flag_modules = [ "Conduit_lwt_unix_ssl", lwt_ssl
                     ; "Conduit_lwt_tls", lwt_tls ]
    ; opt_flags = [ vchan_lwt ; launchd_lwt ] }

  let mirage =
    { name = "conduit-lwt-mirage"
    ; findlib = ["uri.services"]
    ; always_modules = ["Conduit_mirage" ; "Resolver_mirage"]
    ; need_flags = [mirage]
    ; flag_modules = ["Conduit_xenstore", vchan]
    ; opt_flags = [mirage_tls] }

  let all =
    [ conduit
    ; conduit_async
    ; conduit_lwt
    ; conduit_lwt_unix
    ; mirage ]
end

module Test = struct
  type t =
    { pkgs: Pkg.t list (* local libs required *)
    ; flags: Flag.t list (* required flags *)
    ; source_path: string } (* unix/cdtest for example *)

  let should_build t =
    t.pkgs
    |> List.map (fun p -> p.Pkg.need_flags)
    |> List.flatten
    |> List.append t.flags
    |> Flag.uniques
    |> Flag.has_flags

  let cdtest =
    { pkgs = [ Libs.conduit_lwt_unix ]
    ; flags = [ lwt_ssl ]
    ; source_path = "unix/cdtest" }

  let exit_test =
    { cdtest with source_path = "unix/exit_test" }

  let cdtest_tls =
    { cdtest with
      source_path = "unix/cdtest_tls"
    ; flags = [] }

  let all =
    [ cdtest
    ; cdtest_tls
    ; exit_test ]
end

module Target = struct
  type typ = Lib | Bin
  type t =
    { name: string
    ; typ: typ
    ; findlib: string list
    ; modules: string list }

  let tags t =
    match t.findlib with
    | [] -> []
    | _ ->
      let findlib_packages = Ocb.tags_of_packages t.findlib in
      let base =
        match t.typ with
        | Lib -> "lib"
        | Bin -> "tests" in
      let tag mod_ = sprintf "<%s/%s*>: %s"
          base (String.lowercase mod_) findlib_packages in
      List.map tag t.modules

  let write_targets pkg =
    write_lines
      ~file:(sprintf "lib/%s.mllib" pkg.name)
      ~lines:pkg.modules

  let of_pkg p =
    let (extra_modules, extra_flags) =
      p.Pkg.flag_modules
      |> List.filter (fun (_, f) -> Flag.has_flag f)
      |> List.split in
    let all_flags =
      p.Pkg.need_flags @ (Flag.subset_flags (p.Pkg.opt_flags @ extra_flags)) in
    let findlib_from_flags =
      all_flags
      |> List.map (fun x -> x.Flag.findlib_checks)
      |> List.flatten in
    { name = p.Pkg.name
    ; typ = Lib
    ; findlib = List.sort_uniq compare (p.Pkg.findlib @ findlib_from_flags)
    ; modules = p.Pkg.always_modules @ extra_modules }

  let of_test t =
    let findlib =
      t.Test.pkgs
      |> List.map (fun p -> (of_pkg p).findlib)
      |> List.flatten in
    { name = t.Test.source_path
    ; typ = Bin
    ; findlib = findlib
    ; modules = [ t.Test.source_path ] }

  let targets p =
    (match p.typ with
    | Lib -> Ocb.lib_targets
    | Bin -> Ocb.bin_targets) p.name
end

let targets =
  Libs.all
  |> List.filter (fun p -> Flag.has_flags p.Pkg.need_flags)
  |> List.map Target.of_pkg

let test_targets =
  Test.all
  |> List.filter Test.should_build
  |> List.map Target.of_test

let build_targets =
  targets
  |> List.map Target.targets
  |> List.flatten

let build_test_targets =
  test_targets
  |> List.map Target.targets
  |> List.flatten

(* flat list of all modules. necessary to build the docs *)
let doc_modules =
  targets
  |> List.map (fun p -> p.Target.modules)
  |> List.flatten

let tags =
  let extra_tags =
    targets
    |> List.map Target.tags
    |> List.flatten in
  let test_tags =
    test_targets
    |> List.map Target.tags
    |> List.flatten in
  [ sprintf "true: %s" (Ocb.tags_of_packages base_findlib)
  ; "<lib/*>: predicate(custom_ppx), config"
  ; "<lib/*.{ml,mli}>: ppx-driver(ppx_sexp_conv)"
  ; "true: debug,principal,bin_annot,short_paths,thread,strict_sequence"
  ; "<lib>: include" ]
  @ test_tags
  @ extra_tags

let make_meta () =
  let findlib_pkgs = String.concat " " in
  let file f =
    let lines = ref [] in
    let f = open_in f in
    try
      while true do
        lines := (input_line f) :: !lines;
      done;
      assert false
    with End_of_file -> String.concat "\n" !lines
  in
  let target p =
    try
      (List.find (fun n -> n.Target.name = p) targets)
      .Target.findlib
      |> findlib_pkgs
    with Not_found -> ""
  in
  let vars =
    [ "@BASE_REQUIRES@", findlib_pkgs base_findlib
    ; "@VERSION@", String.trim (file "VERSION")
    ; "@ASYNC_REQUIRES@", target "conduit-async"
    ; "@LWT_REQUIRES@", target "conduit-lwt"
    ; "@LWT_UNIX_REQUIRES@", target "conduit-lwt-unix"
    ; "@MIRAGE_REQUIRES@", target "conduit-lwt-mirage" ] in
  vars
  |> List.map (fun (v, r) -> sprintf "-e 's/%s/%s/g'" v r)
  |> String.concat " "
  |> sprintf "sed %s META.in > META"
  |> Sys.command
  |> ignore

let () =
  write_lines ~file:"_tags" ~lines:tags;
  write_lines ~file:"lib/conduit_config.mlh"
    ~lines:(Lazy.force Flag.mlh_config);
  write_lines ~file:"lib/conduit.odocl" ~lines:doc_modules;
  write_lines ~file:"conduit.itarget" ~lines:build_targets;
  write_lines ~file:"tests.itarget" ~lines:build_test_targets;
  targets |> List.iter Target.write_targets;
  make_meta ()
