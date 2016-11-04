.PHONY: all install clean doc github

EXT_OBJ=`ocamlfind ocamlc -config | awk '/^ext_obj:/ {print $2}'`
EXT_LIB=`ocamlfind ocamlc -config | awk '/^ext_lib:/ {print $2}'`

OCAMLBUILD = ocamlbuild -use-ocamlfind -classic-display -no-links \
	-cflags "-w A-4-33-40-41-42-43-34-44"

PREFIX ?= /usr/local/bin
OS_TYPE:=$(shell ocamlfind ocamlc -config | awk '/^os_type:/ {print $$2}')
ifeq ($(OS_TYPE),$(filter $(OS_TYPE),Win32 Cygwin))
EXT_EXE=.exe
else
EXT_EXE=
endif


B=_build/lib
FILES = $(wildcard $B/*.cmi $B/*.cmt $B/*.cmti $B/*.cmx $B/*.cmxa $B/*.cma $B/*.cmxs $B/$B/*$(EXT_LIB) $B/*$(EXT_OBJ) $B/*.cmo)
MORE_FILES = $(wildcard lib/intro.html $B/*.mli)

all: ppx
	$(OCAMLBUILD) conduit.otarget

install: ppx
	rm -rf _install
	mkdir -p _install
ifneq ("$(wildcard _build/lib/conduit_xenstore.cmo)","")
	echo '"scripts/xenstore-conduit-init" {"xenstore-conduit-init"}' > _install/bin
endif
	$(foreach f,$(FILES), echo "$(f)" >> _install/lib;)
	ocamlfind remove conduit || true
	ocamlfind install conduit META $(FILES) $(MORE_FILES)

clean:
	$(OCAMLBUILD) -clean
	rm -rf _install ppx lib/conduit_config.mlh META _tags

doc: ppx
	$(OCAMLBUILD) lib/conduit.docdir/index.html

github: doc
	git checkout gh-pages
	git merge master --no-edit
	$(MAKE)
	rm -f *.html
	cp _build/lib/conduit-all.docdir/* .
	git add *.html
	cp nice-style.css style.css
	git add style.css
	git commit -m 'sync ocamldoc' *.html *.css
	git push
	git checkout master

VERSION = $(shell cat VERSION)
NAME    = conduit
ARCHIVE = https://github.com/mirage/ocaml-$(NAME)/archive/v$(VERSION).tar.gz

release:
	git tag -a v$(VERSION) -m "Version $(VERSION)."
	git push upstream v$(VERSION)
	$(MAKE) pr

pr:
	opam publish prepare $(NAME).$(VERSION) $(ARCHIVE)
	OPAMYES=1 opam publish submit $(NAME).$(VERSION) && rm -rf $(NAME).$(VERSION)

ppx:
	ocamlfind ocamlopt -predicates ppx_driver -o ppx$(EXT_EXE) -linkpkg \
	  -package ppx_sexp_conv ppx_driver_runner.cmxa
