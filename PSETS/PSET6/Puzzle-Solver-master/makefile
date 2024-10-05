all: collections puzzlesolve tests experiments

collections: collections.ml
	ocamlbuild -use-ocamlfind collections.byte
puzzlesolve: puzzlesolve.ml
	ocamlbuild -use-ocamlfind puzzlesolve.byte
tests: tests.ml
	ocamlbuild tests.byte
experiments: experiments.ml
	ocamlbuild experiments.byte
clean:
	rm -rf _build *.byte
