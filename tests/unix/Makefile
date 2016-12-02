clean:
	ocamlbuild -clean

cdtest.native: cdtest.ml
	ocamlbuild ./cdtest.native -use-ocamlfind -tag 'package(conduit.lwt-unix)' -tag debug

cdtest_tls.native: cdtest.ml
	ocamlbuild ./cdtest_tls.native -use-ocamlfind -tag 'package(conduit.lwt-unix)' -tag debug
