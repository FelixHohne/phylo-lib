MODULES=tree lexer phylo_parser dna pairwise msa distance phylo_algo authors phylo_printer
OBJECTS=$(MODULES:=.cmo)
TESTS = phylo_algo_test distance_test tree_test lexer_test phylo_parser_test dna_test pairwise_test msa_test phylo_printer_test
TESTOBJECTS = $(TESTS:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
MAIN=phylo_parser.byte 
OCAMLBUILD=ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)'

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)
	$(OCAMLBUILD) $(TESTOBJECTS)
	$(OCAMLBUILD) sample_trees.cmo

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

test:
	$(OCAMLBUILD) test.byte && ./test.byte

bisect:
	BISECT_COVERAGE=YES ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)' test.byte
	./test.byte -runner sequential
	bisect-ppx-report -I _build -html report bisect0001.out


docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report phylo_lib_src.zip bisect*.out bisect*.coverage

zip:
	zip phylo_lib_src.zip *.ml* _tags Makefile FASTA/* PhyloXML/* resources/* viruses/* .ocamlinit INSTALL.txt