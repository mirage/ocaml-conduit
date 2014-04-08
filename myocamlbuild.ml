open Ocamlbuild_plugin ;;
dispatch (
  function
  | After_rules ->
    pflag ["ocaml";"compile";] "define" (fun s -> S [A"-ppopt"; A ("-D"^s)]);
    pflag ["ocaml";"ocamldep";] "define" (fun s -> S [A"-ppopt"; A ("-D"^s)])
  | _ -> ()
)
