build:
	ocamlbuild -use-ocamlfind tree.cmo tree_test.cmo
	ocamlbuild -use-ocamlfind dna.cmo dna_test.cmo
	ocamlbuild -use-ocamlfind phylo_parser.cmo phylo_parser_test.cmo
	ocamlbuild -use-ocamlfind lexer.cmo
	ocamlbuild -use-ocamlfind sample_trees.cmo


test:
	ocamlbuild -use-ocamlfind -tag 'debug' tree_test.byte && ./tree_test.byte
	ocamlbuild -use-ocamlfind -tag 'debug' dna_test.byte && ./dna_test.byte
	ocamlbuild -use-ocamlfind -tag 'debug' phylo_parser_test.byte && ./phylo_parser_test.byte



docs:
	mkdir -p doc
	ocamldoc -d doc -html tree.mli
	ocamldoc -d doc -html phylo_parser.mli
	ocamldoc -d doc -html dna.mli



clean:
	ocamlbuild -clean
	rm -rf doc
