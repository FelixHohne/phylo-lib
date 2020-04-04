(* Idea: Create a series of Logic Tokens, as an intermediary representation 
between the lexer tokens and the actual tree. 

Checks for syntax errors and makes creating a tree easier, since there is 
now a higher level structure to convert into a tree. *)

(* Examples: *)

type root_status = {rooted : bool}

type name = {name :  string}

type bootstrap_value = {confidence : int }

type taxonomy = {code : string} 

type logicToken = OpenPhylo of root_status | ClosePhylo | Name of name 
| ScientificName of name | Description of name | OpenEvents 
| CloseEvents | Speciations of int | OpenClade 
| OpenCladeWithName of name | CloseClade | Taxonomy of taxonomy | Sequence 










