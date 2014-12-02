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
