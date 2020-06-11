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
**Running the XML Parser:** Parse an amphibian species phyloXML file found in the Phylo folder 
   called frog.xml into our custom built n-ary tree, then pretty-print it using ASCII art.  
   
  ```OCaml
  let phylo1 = Phylo_parser.from_phylo "PhyloXML/frog.xml";;
  Tree.print_tree phylo1.tree;;
   ```
<br> <br>
**Pairwise Alignment using the Needleman-Wunsch algorithm:** The Needleman-Wunsch algorithm is a globally optimal algorithm for finding the pairwise alignment of two strings using dynamic programming. Here we implement it to find an optimal alignment of two pairs of DNA sequences. 

```OCaml
let dna1 = Dna.from_string "AATCGTAGGCCCC";; 
let dna2 = Dna.from_string "ATTGCGACTCGTATC";; 
let arr = Pairwise.align_pair dna1 dna2 1 (-1) (-1) |> fst;;
Dna.to_string arr.(0);;
Dna.to_string arr.(1);;
```
