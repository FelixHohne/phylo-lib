OCAMLBUILD=ocamlbuild -use-ocamlfind

build:
	$(OCAMLBUILD) tree.cmo tree_test.cmo
	$(OCAMLBUILD) lexer.cmo lexer_test.cmo
	$(OCAMLBUILD) phylo_parser.cmo phylo_parser_test.cmo
	$(OCAMLBUILD) sample_trees.cmo
	$(OCAMLBUILD) dna.cmo dna_test.cmo


test:
	$(OCAMLBUILD) -tag 'debug' tree_test.byte && ./tree_test.byte
	$(OCAMLBUILD) -tag 'debug' lexer_test.byte && ./lexer_test.byte
	$(OCAMLBUILD) -tag 'debug' phylo_parser_test.byte && ./phylo_parser_test.byte
	$(OCAMLBUILD) -tag 'debug' dna_test.byte && ./dna_test.byte

docs:
	mkdir -p doc
	ocamldoc -d doc -html tree.mli
	# ocamldoc -d doc -html phylo_parser.mli
	ocamldoc -d doc -html dna.mli
	ocamldoc -d doc -html lexer.mli



clean:
	ocamlbuild -clean
	rm -rf doc
