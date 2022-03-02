.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop lib

test:
	dune runtest

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean

doc:
	dune build @doc
