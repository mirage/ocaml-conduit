open Ocamlbuild_plugin ;;
dispatch (
  function
  | After_rules ->
    pflag ["ocaml";"compile";] "define" (fun s -> S [A"-ppopt"; A ("-D"^s)]);
    pflag ["ocaml";"ocamldep";] "define" (fun s -> S [A"-ppopt"; A ("-D"^s)]);
    dep ["ocaml"; "doc"] ["lib/intro.html"];
    flag ["ocaml"; "doc"] (S[A"-short-functors"; A"-sort"; A"-m"; A"A"; A"-intro"; A"lib/intro.html"]);
  | _ -> ()
)
