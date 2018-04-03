structure Main =
struct 

(* for creating lexer and parser *)
structure mdtabLrVals = mdtabLrValsFun(structure Token = LrParser.Token); 
structure mdtabLex = mdtabLexFun(structure Tokens = mdtabLrVals.Tokens);
structure mdtabParser = Join(
    structure LrParser = LrParser 
    structure ParserData = mdtabLrVals.ParserData
    structure Lex = mdtabLex
    ) 

fun parse_stream stream = 
    let val lexer = mdtabParser.makeLexer (fn i => TextIO.inputN(stream, i))
        val (ast, _) = mdtabParser.parse(0, lexer, fn(_,_,_) => print "error\n", ())
    in 
        ast
    end 

fun parse_file f = 
    let val s = TextIO.opneIn f
        val r = parse_stream s 
    in 
        (r before TextIO.closeIn s; print("Success!\n"))
    end 
    handle _ => print("Fail!\n")
    
fun parse_string s = (parse_stream (TextIO.openString s))

fun main () =
    let fun usage () = print "Markdown+Tables for CSL226 Assignment 2. \nUsage:markdown \"filename\"\n"
    in  usage()
    end
end