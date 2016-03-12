open Mirage

let main = foreign "Unikernel.Server" (console @-> job)

let () =
  add_to_ocamlfind_libraries ["conduit.mirage"; "vchan.xen"];
  add_to_opam_packages ["conduit"; "vchan"];
  register "vchan_server" [ main $ default_console ]
