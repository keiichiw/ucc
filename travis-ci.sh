# OPAM packages needed to build tests
export OPAM_PACKAGES='ocamlfind menhir'

# install ocaml from apt
echo "yes" | sudo add-apt-repository ppa:avsm/ocaml41+opam11
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers opam

echo OCaml version
ocaml -version

# install packages from opam
opam init
opam update
opam install -q -y ${OPAM_PACKAGES}

# compile & run tests
make test
