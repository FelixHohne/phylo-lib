build:
	ocamlbuild -use-ocamlfind tree.cmo tree_test.cmo
	ocamlbuild -use-ocamlfind dna.cmo dna_test.ml

test:
	ocamlbuild -use-ocamlfind -tag 'debug' tree_test.byte && ./tree_test.byte
	ocamlbuild -use-ocamlfind -tag 'debug' dna_test.byte && ./dna_test.byte


docs:
	mkdir -p doc
	ocamldoc -d doc -html tree.mli

clean:
	ocamlbuild -clean
	rm -rf doc
