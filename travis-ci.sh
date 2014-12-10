# OPAM packages needed to build tests
export OPAM_PACKAGES='ocamlfind'

# install ocaml from apt
echo "yes" | sudo add-apt-repository ppa:avsm/ocaml41+opam11
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers opam menhir

ocaml -version

# install packages from opam
opam init -y
opam update -y
opam install -q -y ${OPAM_PACKAGES}

# compile & run tests
make test
