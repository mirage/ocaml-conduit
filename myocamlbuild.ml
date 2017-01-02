open Ocamlbuild_plugin

let () =
  dispatch (fun hook ->
      Ppx_driver_ocamlbuild.dispatch hook;
      match hook with
      | After_rules ->
        dep ["ocaml"; "config"] ["lib/conduit_config.mlh"];
        dep ["ocaml"; "doc"] ["lib/intro.html"];
        flag ["ocaml"; "doc"]
          (S [ A "-hide-warnings" ; A "-short-functors"
             ; A "-sort" ; A "-m" ; A"A" ; A"-intro"
             ; A"lib/intro.html" ; A"-t"
             ; A"Conduit URI resolution" ]);
      | _ -> ()
    )
