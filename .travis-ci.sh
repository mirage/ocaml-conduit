OPAM_DEPENDS="sexplib ipaddr cstruct stringext uri"

case "$OCAML_VERSION,$OPAM_VERSION" in
4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time libssl-dev

export OPAMYES=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam init -a
opam install ${OPAM_DEPENDS}

eval `opam config env`
make

opam install lwt
make clean
make
make install

opam remove lwt
opam install async
make clean
make
make install

export OPAMVERBOSE=1
opam install async_ssl
make clean
make
make install

export OPAMVERBOSE=0
opam install lwt
make clean
make
make install

opam install ssl
make clean
make
make install

opam install cohttp cowabloga
