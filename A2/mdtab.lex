datatype lexresult = HTML | EOF

val linenum = ref 1
val error = fn x => output(stdOut,x ^ "\n")
val eof = fn () => EOF
val file = openOut "output.html"

%%

%structure mdTabLex

tagName = [A-Za-z]+;
ws = [\ \t];

%s START_TAG_BEG;
%s START_TAG_END; 
%s END_TAG_BEG; 
%s END_TAG_END;
%s HTML_CONTENT;

%%

\n                          => ((!linenum)=(!linenum)+1; lex());
{ws}+                       => (lex());
<INITIAL>"<"                => (YYBEGIN START_TAG_BEG; output(file, yytext); lex());
<START_TAG_BEG>{tagName}    => (YYBEGIN START_TAG_END; output(file, yytext); lex());
<START_TAG_END>">"          => (YYBEGIN HTML_CONTENT; output(file, yytext); lex());
<HTML_CONTENT>[^\<]*        => (YYBEGIN END_TAG_BEG; output(file, yytext); lex());
<END_TAG_BEG>"<"{ws}*"/"    => (YYBEGIN END_TAG_END; output(file, "</"); lex());
<END_TAG_END>{tagName}      => (output(file, yytext); lex());
<END_TAG_END>">"            => (YYBEGIN INITIAL; output(file, ">"); closeOut(file); HTML);
.                           => (error("ignoring bad character:"^yytext); EOF);