clean:
	ocamlbuild -clean
cdtest.native: cdtest.ml
	ocamlbuild ./cdtest.native -use-ocamlfind -tag 'package(conduit.lwt-unix)' -tag debug
