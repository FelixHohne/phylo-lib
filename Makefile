MODULES=tree lexer phylo_parser dna pairwise msa distance phylo_algo authors
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
MAIN=phylo_parser.byte 
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)
	$(OCAMLBUILD) sample_trees.cmo

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

bisect:
	BISECT_COVERAGE=YES ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)' pairwise_test.byte
	./pairwise_test.byte -runner sequential
	bisect-ppx-report -I _build -html report bisect0001.out
	BISECT_COVERAGE=YES ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)' lexer_test.byte
	./lexer_test.byte -runner sequential
	bisect-ppx-report -I _build -html report bisect0002.out
	BISECT_COVERAGE=YES ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)' phylo_parser_test.byte
	./phylo_parser_test.byte -runner sequential
	bisect-ppx-report -I _build -html report bisect0003.out
	BISECT_COVERAGE=YES ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)' phylo_algo_test.byte
	./phylo_algo_test.byte -runner sequential
	bisect-ppx-report -I _build -html report bisect0004.out
	BISECT_COVERAGE=YES ocamlbuild -use-ocamlfind -plugin-tag 'package(bisect_ppx-ocamlbuild)' dna_test.byte
	./dna_test.byte -runner sequential
	bisect-ppx-report -I _build -html report bisect0005.out


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
	rm -rf doc.public doc.private report bisect*.out bisect*.coverage

zip:
	zip phylo_lib_src.zip *.ml* _tags Makefile FASTA PhyloXML resources .ocamlinit INSTALL.txt