structure T = Tokens
 
type lexresult = Tokens.token
type pos = int 

val error = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof = (fn () => Tokens.EOF)

%%
%posarg 

tagName = [A-Za-z]+;
ws = [\ \t];

%s START_TAG_BEG;
%s START_TAG_END; 
%s END_TAG_BEG; 
%s END_TAG_END;
%s HTML_CONTENT;

%%

\n                          => (continue());
{ws}+                       => (continue());
<INITIAL>"<"                => (YYBEGIN START_TAG_BEG; T.START_TAG_BEG(yypos));
<START_TAG_BEG>{tagName}    => (YYBEGIN START_TAG_END; T.START_TAG_NAME(yytext, yypos));
<START_TAG_END>">"          => (YYBEGIN HTML_CONTENT; T.START_TAG_END(yypos));
<HTML_CONTENT>[^\<]*        => (YYBEGIN END_TAG_BEG; T.HTML_CONTENT(yytext, yypos));
<END_TAG_BEG>"<"{ws}*"/"    => (YYBEGIN END_TAG_END; T.END_TAG_BEG(yypos));
<END_TAG_END>{tagName}      => (T.END_TAG_NAME(yytext, yypos));
<END_TAG_END>">"            => (YYBEGIN INITIAL; T.END_TAG_END(yypos));
.                           => (error("ignoring bad character:"^yytext); T.EOF);
