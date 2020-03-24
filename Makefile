build:
	ocamlbuild -use-ocamlfind tree.cmo tree_test.cmo

test:
	ocamlbuild -use-ocamlfind -tag 'debug' tree_test.byte && ./tree_test.byte

docs:
	mkdir -p doc
	ocamldoc -d doc -html tree.ml

clean:
	ocamlbuild -clean
	rm -rf doc
