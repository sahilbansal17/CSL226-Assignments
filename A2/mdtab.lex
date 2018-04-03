structure T = Tokens
 
type lexresult = Tokens.token
type pos = int 

val error = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof = (fn () => Tokens.EOF)

(* to use yypos, need to pass starting argument in makeLexer *)
%%
%posarg 

tagName = [A-Za-z]+;
ws = [\ \t];

%%

\n                          => (continue());
{ws}+                       => (continue());

"<"                         => (T.START_TAG_BEG(yypos));
"<"{ws}*"/"                 => (T.END_TAG_BEG(yypos));

{tagName}                   => (T.TAG_NAME(yytext, yypos));

">"                         => (T.TAG_END(yypos));

[^\<>*_#"---"]*             => (T.HTML_CONTENT(yytext, yypos));

"*"                         => (T.ITALICS(yypos));
"**"                        => (T.BOLD(yypos));
"_"                         => (T.UNDERLINE(yypos));
"#"                         => (T.H1(yypos));
"##"                        => (T.H2(yypos));
"###"                       => (T.H3(yypos));
"####"                      => (T.H4(yypos));
"#####"                     => (T.H5(yypos));
"######"                    => (T.H6(yypos));
"---"                       => (T.HR(yypos));
.                           => (error("ignoring bad character:"^yytext); T.EOF);