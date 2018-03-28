(* 

To run the program :
1. ml-lex mdtab.lex 
2. use "test.sml"
3. lexer();
4. Now, use the following as input, gives the same output as in output.html
<    body    > Hello world  <    /     body    >  

*)

open TextIO;
use "mdtab.lex.sml";
(* open mdTabLex; *)

(* 

need to make some fixes to take input from a file 

val inputFile = openIn("test.txt");
val l = inputLine(inputFile);
closeIn(inputFile);
val lexer = makeLexer(fn n => valOf(l)); 

*)

val lexer = mdTabLex.makeLexer(fn n => valOf(inputLine(stdIn)));
