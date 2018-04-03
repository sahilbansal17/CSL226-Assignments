(* 

To run the program :
1. ml-lex mdtab.lex 
2. use "test.sml"
3. markdown "fileName"
*)

(* Tokens *)
use "tokens.sml";

(* Lexer *)
use "mdtab.lex.sml";

(* Main *)
use "main.sml";

(* parse function accessible through markdown variable *)
val markdown = Main.parse;

(* Display usage instructions *)
Main.main();