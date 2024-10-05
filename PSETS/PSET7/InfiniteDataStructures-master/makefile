all: refs refs_test music nativeLazyStreams streamstrees

askshiebs_tests: askshiebs_tests.ml
	ocamlbuild -use-ocamlfind askshiebs_tests.byte

refs: refs.ml
	ocamlbuild -use-ocamlfind refs.byte

refs_test: refs_test.ml
	ocamlbuild -use-ocamlfind refs_test.byte

music: music.ml
	ocamlbuild -use-ocamlfind music.byte

nativeLazyStreams: nativeLazyStreams.ml
	ocamlbuild -use-ocamlfind nativeLazyStreams.byte

streamstrees: streamstrees.ml
	ocamlbuild -use-ocamlfind streamstrees.byte

clean:
	rm -rf _build *.byte