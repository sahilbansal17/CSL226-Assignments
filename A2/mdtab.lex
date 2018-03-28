datatype lexresult = HTML | EOF

val linenum = ref 1
val error = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof = fn () => EOF

%%

%structure mdTabLex

tagName = [A-Za-z];
ws = [\ \t];

%s START_TAG_BEG;
%s START_TAG_END; 
%s END_TAG_BEG; 
%s END_TAG_END;
%s HTML_CONTENT;

%%

\n                          => ((!linenum)=(!linenum)+1; lex());
{ws}+                       => (lex());
<INITIAL>"<"                => (YYBEGIN START_TAG_BEG; print(yytext); lex());
<START_TAG_BEG>{tagName}    => (YYBEGIN START_TAG_END; print(yytext); lex());
<START_TAG_END>">"          => (YYBEGIN HTML_CONTENT; print(yytext); lex());
<HTML_CONTENT>.             => (YYBEGIN END_TAG_BEG; print(yytext); lex());
<END_TAG_BEG>"<"{ws}*"/"    => (YYBEGIN END_TAG_END; print("</"); lex());
<END_TAG_END>{tagName}      => (print(yytext); lex());
<END_TAG_END>">"            => (YYBEGIN INITIAL; print(">"); HTML);
.                           => (error ("calc: ignoring bad character "^yytext); lex());