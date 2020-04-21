MODULES=tree lexer phylo_parser sample_trees dna pairwise msa distance phylo_algo
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
MAIN=phylo_parser.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop
	
build:
	$(OCAMLBUILD) $(OBJECTS)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

test:
	$(OCAMLBUILD) -tag 'debug' tree_test.byte && ./tree_test.byte
	$(OCAMLBUILD) -tag 'debug' lexer_test.byte && ./lexer_test.byte
	$(OCAMLBUILD) -tag 'debug' phylo_parser_test.byte && ./phylo_parser_test.byte
	$(OCAMLBUILD) -tag 'debug' dna_test.byte && ./dna_test.byte
	$(OCAMLBUILD) -tag 'debug' pairwise_test.byte && ./pairwise_test.byte
	$(OCAMLBUILD) -tag 'debug' msa_test.byte && ./msa_test.byte
	$(OCAMLBUILD) -tag 'debug' distance_test.byte && ./distance_test.byte
	$(OCAMLBUILD) -tag 'debug' phylo_algo_test.byte && ./phylo_algo_test.byte


docs:
	mkdir -p doc
	ocamldoc -d doc -html tree.mli
	ocamldoc -d doc -html dna.mli
	ocamldoc -d doc -html lexer.mli
	# ocamldoc -d doc -html phylo_parser.mli
	ocamldoc -d doc -html clustal.mli
	ocamldoc -d doc -html distance.mli

clean:
	ocamlbuild -clean
	rm -rf doc
