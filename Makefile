.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

solo-play:
	OCAMLRUNPARAM=b dune exec bin/solo_game.exe

combos:
	OCAMLRUNPARAM=b dune exec bin/combs_printer.exe

zip:
	rm -f 24_battle_royal
	zip -r 24_battle_royale.zip . 

clean:
	dune clean
	rm -f 24_battle_royale.zip

doc:
	dune build @doc
