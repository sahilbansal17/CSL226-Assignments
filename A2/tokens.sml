structure Tokens =
struct 

datatype token =
    (* < *)
	 START_TAG_BEG of int
    (* > *)
   | START_TAG_NAME of string*int 
   | START_TAG_END of int 
    (* ... *)
   | HTML_CONTENT of string*int
    (* </ *)
   | END_TAG_BEG of int
    (* > *)
   | END_TAG_NAME of string*int 
   | END_TAG_END of int
   | ITALICS_BEGIN of int 
   | BOLD_BEGIN of int 
   | UNDERLINE_BEGIN of int
   | ITALICS_END of int 
   | BOLD_END of int 
   | UNDERLINE_END of int 
   | EOF

fun toString token =
	case token of 
		  START_TAG_BEG (pos) 		=> "< AT POS:" ^ Int.toString pos
		| START_TAG_NAME(text, pos)	=> text^" AT POS:" ^ Int.toString pos
		| START_TAG_END (pos) 		=> "> AT POS:" ^ Int.toString pos  
		| HTML_CONTENT (text, pos) 	=> text^" AT POS:" ^ Int.toString pos  
		| END_TAG_BEG (pos) 		=> "</ AT POS:" ^ Int.toString pos
		| END_TAG_NAME(text, pos)	=> text^" AT POS:" ^ Int.toString pos
		| END_TAG_END (pos) 		=> "> AT POS:" ^ Int.toString pos  
		| ITALICS_BEGIN (pos) 		=> "* AT POS:" ^ Int.toString pos  
		| BOLD_BEGIN (pos) 			=> "** AT POS:" ^ Int.toString pos  
		| UNDERLINE_BEGIN (pos) 	=> "_ AT POS:" ^ Int.toString pos  
		| ITALICS_END (pos)	 		=> "* AT POS:" ^ Int.toString pos  
		| BOLD_END (pos) 			=> "** AT POS:" ^ Int.toString pos  
		| UNDERLINE_END (pos) 		=> "_ AT POS:" ^ Int.toString pos 
		| EOF						=> "EOF"
end