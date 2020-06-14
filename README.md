# An OCaml Phylogenetic Tree Library

### Spring 2020 
### by Shiyuan Huang, Felix Hohne, and Vaishnavi Gupta

___ 

### Activity Breakdown 
- Parsing DNA Files: Felix Hohne
- Lexing PhyloXML Files: trio-programmed 
- N-ary trees: Vaishnavi Gupta
- Tree Pretty Printing: Vaishnavi Gupta
- Phylo_Parser to parse PhyloXML Files: Shiyuan Huang the rest was trio-programmed 
- Construction of Distance Matrices for UPGAM: trio-programmed 
- Pairwise alignment using Dynamic Programming : trio-programmed
- Phylogenetic Algorithms: trio-programmed

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
### Functionality: 

- Parsing existing phyloXML phylogenetic tree files into n-ary trees
- Displaying phylogenetic trees in ASCII format
- Parsing DNA base pair sequences from FASTA files
- Construction of phylogenetic trees from DNA sequences using Needleman-Wunsch for pairwise alignment and UPGMA to construct rooted phylogenetic trees
- Outputting the constructed phylogenetic trees in the form of XML-format files to support compatibility with other biocomputational programs

___ 
### Simple Examples 
**Running the XML Parser:** Parse an amphibian species phyloXML file found in the Phylo folder 
   called frog.xml into our custom built n-ary tree, then pretty-print it using ASCII art.  
   
  ```OCaml
  let phylo1 = Phylo_parser.from_phylo "PhyloXML/frog.xml"
  Tree.print_tree phylo1.tree
   ```

**Pairwise Alignment using the Needleman-Wunsch algorithm:** The Needleman-Wunsch algorithm is a globally optimal algorithm for finding the pairwise alignment of two strings using dynamic programming. Here we implement it to find an optimal alignment of two pairs of DNA sequences. 

```OCaml
let dna1 = Dna.from_string "AATCGTAGGCCCC"
let dna2 = Dna.from_string "ATTGCGACTCGTATC"
let arr = Pairwise.align_pair dna1 dna2 1 (-1) (-1) |> fst
Dna.to_string arr.(0)
Dna.to_string arr.(1)
```

**Construct a phylogenetic tree from DNA .FASTA Files**:We construct a phylogenetic tree for the H1N1, H5N1, and H3N2 viruses using  their PB-2 gene data. The resulting tree shows that H1N1 and H3N2 are more 
closely related, which makes sense since they are swine flus while H5N1 is an 
avian flu.

```OCaml
let d1 = Dna.from_fasta "viruses/h5n1.fasta"
let d2 = Dna.from_fasta "viruses/h1n1.fasta"
let d3 = Dna.from_fasta "viruses/h3n2.fasta"
let mat = Distance.dist_dna [| d1; d2; d3 |] 1 (-1) (-1)
Phylo_algo.upgma mat [|"H5N1"; "H1N1"; "H3N2"|] |> Tree.print_tree
```
