open Ocamlbuild_plugin ;;
dispatch (
  function
  | After_rules ->
    pflag ["ocaml";"compile";] "define" (fun s -> S [A"-ppopt"; A ("-D"^s)]);
    pflag ["ocaml";"ocamldep";] "define" (fun s -> S [A"-ppopt"; A ("-D"^s)]);
    flag ["ocaml"; "doc"] (S[A"-short-functors"; A"-sort"; A"-m"; A"A"]);
  | _ -> ()
)
