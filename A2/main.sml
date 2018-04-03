structure Main =
struct 

structure T=Tokens 

fun parse fname = 
    let val ins = TextIO.openIn fname
        val lexer = Mlex.makeLexer ((fn _ => TextIO.input ins),~1)
        fun loop() =
            let val token = lexer()
            in 
		          print((T.toString token)^"\n");
		          case token of 
		                 T.EOF => ()
		               | _ => loop()
            end
    in 
        loop() before TextIO.closeIn ins
    end

fun main () =
    let fun usage () = print "Markdown+Tables for CSL226 Assignment 2. \nUsage:markdown \"filename\"\n"
    in  usage()
    end
end
