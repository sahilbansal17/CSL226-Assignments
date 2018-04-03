structure Tokens =
struct 

datatype token =
    (* < *)
	 START_TAG_BEG of int
    (* > *)
   | TAG_NAME of string*int 
   | TAG_END of int 
    (* ... *)
   | HTML_CONTENT of string*int
    (* </ *)
   | END_TAG_BEG of int
    (* > *)
   | ITALICS of int 
   | BOLD of int 
   | UNDERLINE of int
   | H1 of int 
   | H2 of int 
   | H3 of int 
   | H4 of int 
   | H5 of int 
   | H6 of int 
   | HR of int 
   | EOF

fun toString token =
	case token of 
		  START_TAG_BEG (pos) 		=> "< AT POS:" ^ Int.toString pos
		| TAG_NAME(text, pos)		=> text^" AT POS:" ^ Int.toString pos
		| TAG_END (pos) 			=> "> AT POS:" ^ Int.toString pos  
		| HTML_CONTENT (text, pos) 	=> text^" AT POS:" ^ Int.toString pos  
		| END_TAG_BEG (pos) 		=> "</ AT POS:" ^ Int.toString pos  
		| ITALICS (pos) 			=> "* AT POS:" ^ Int.toString pos  
		| BOLD (pos) 				=> "** AT POS:" ^ Int.toString pos  
		| UNDERLINE (pos) 			=> "_ AT POS:" ^ Int.toString pos
		| H1 (pos)					=> "H1 at POS:" ^ Int.toString pos  
		| H2 (pos)					=> "H2 at POS:" ^ Int.toString pos  
		| H3 (pos)					=> "H3 at POS:" ^ Int.toString pos  
		| H4 (pos)					=> "H4 at POS:" ^ Int.toString pos  
		| H5 (pos)					=> "H5 at POS:" ^ Int.toString pos  
		| H6 (pos)					=> "H6 at POS:" ^ Int.toString pos
		| HR (pos)					=> "HR at POS:" ^ Int.toString pos  
		| EOF						=> "EOF"
end