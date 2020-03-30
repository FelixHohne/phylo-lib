(** The type of lexer tokens. *)
type token =
| Phylogeny | Name | Description
| Clade | Rank | Confidence
| Taxonomy | SciName | ID
| LAngle | Slash | RAngle | Quote | Eq | Num of int | Dot
| Word of string | True | False  