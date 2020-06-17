MODULES=tree lexer phylo_parser dna pairwise msa distance phylo_algo phylo_printer
OBJECTS=$(MODULES:=.cmo)
TESTS = phylo_algo_test distance_test tree_test lexer_test phylo_parser_test dna_test pairwise_test msa_test phylo_printer_test
TESTOBJECTS = $(TESTS:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)

MAIN=phylo_parser.byte 
OCAMLBUILD=ocamlbuild -use-ocamlfind -I src -plugin-tag 'package(bisect_ppx-ocamlbuild)'

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)
	$(OCAMLBUILD) $(TESTOBJECTS)
	$(OCAMLBUILD) sample_trees.cmo

test:
	$(OCAMLBUILD) test.byte && ./test.byte

bisect:
	BISECT_COVERAGE=YES ocamlbuild -use-ocamlfind -I src -plugin-tag 'package(bisect_ppx-ocamlbuild)' test.byte
	./test.byte -runner sequential
	bisect-ppx-report -I _build/src -html report bisect0001.out


docs: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build/src \
		-html -stars -d doc.public src/*.ml[i]

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private report testfile.xml phylo_lib_src.zip bisect*.out bisect*.coverage src/testfile.xml

zip:
	zip phylo_lib_src.zip _tags Makefile .merlin .ocamlinit FASTA/* INSTALL.txt myocamlbuild.ml src/* PhyloXML/* README.md resources/* viruses/*  