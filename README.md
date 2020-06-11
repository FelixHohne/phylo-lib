# An OCaml Phylogenetic Tree Library 

### Spring and Summer 2020 
### by Shiyuan Huang, Felix Hohne, and Vaishnavi Gupta

___ 

### What are phylogenetic trees?

Scientists often wish to infer evolutionary history between different organisms.
In order to determine the closeness of species such as birds or fish, 
historically the similarity in physical characteristics was analyzed. 
With the advent of modern computers and DNA analysis, the field has moved 
towards using DNA. 

Two species with more similar DNA are assumed to be more closely related. By 
analyzing these similarities and differences in the DNA, we can generate a 
hypothetical evolutionary tree, called a phylogenetic tree, that estimates, to 
the best of our ability, what the actual historical evolutionary tree would have 
looked like. 

___ 
### Algorithms used 


___ 
### Examples 
1. _Running the XML Parser:_ Parse an amphibian species phyloXML file found in the Phylo folder 
   called frog.xml into our custom built n-ary tree, then pretty-print it using ASCII art.
   
  let phylo1 = Phylo_parser.from_phylo "PhyloXML/frog.xml";;
  Tree.print_tree phylo1.tree;;



