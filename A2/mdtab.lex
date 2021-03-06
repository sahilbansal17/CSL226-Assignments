structure T = Tokens 
 
(* ML-Lex requires these types and values to be defined *) 
type pos = int 
type svalue = T.svalue 
type ('a,'b) token = ('a, 'b) T.token 
type lexresult = (svalue, pos) token
val eof = (fn () => T.EOF(0, 0))
 
val error = (fn (x,y) => TextIO.output(TextIO.stdOut,x ^ "\n"))

(* to use yypos, need to pass starting argument in makeLexer *)
(* to use Tokens from ML-Yacc, need to define header with functor having structure generated by yacc as argument *)

%%
%posarg 
%header (functor mdtabLexFun(structure Tokens : mdtab_TOKENS));

tagName = [A-Za-z]+;
ws = [\ \t];

%%

\n                          => (continue());
{ws}+                       => (continue());

"<"                         => (T.START_TAG_BEG(yypos,yypos + size yytext));
"<"{ws}*"/"                 => (T.END_TAG_BEG(yypos,yypos + size yytext));

{tagName}                   => (T.TAG_NAME(yytext, yypos,yypos + size yytext));

">"                         => (T.TAG_END(yypos,yypos + size yytext));

[^\<>*_]*             => (T.HTML_CONTENT(yytext, yypos,yypos + size yytext));

"*"                         => (T.ITALICS(yypos,yypos + size yytext));
"**"                        => (T.BOLD(yypos,yypos + size yytext));
"_"                         => (T.UNDERLINE(yypos,yypos + size yytext));
"#"                         => (T.H1(yypos,yypos + size yytext));
"##"                        => (T.H2(yypos,yypos + size yytext));
"###"                       => (T.H3(yypos,yypos + size yytext));
"####"                      => (T.H4(yypos,yypos + size yytext));
"#####"                     => (T.H5(yypos,yypos + size yytext));
"######"                    => (T.H6(yypos,yypos + size yytext));
.                           => (error("ignoring bad character:"^yytext, yypos); T.EOF(yypos, yypos + size yytext));