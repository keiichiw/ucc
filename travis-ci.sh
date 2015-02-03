# install ocaml from apt
yes yes | sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers

ocaml -version

# compile & run tests
make test-all
