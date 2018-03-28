open mdTabLex;
val lexer = makeLexer(fn n => valOf(inputLine(openIn("test.txt"))));