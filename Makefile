.PHONY: all install clean doc github

PREFIX ?= /usr/local/bin

all:
	@./build.sh

install:
	@./build.sh true

clean:
	rm -rf _build _install

doc:
	@BUILD_DOC=true ./build.sh

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
