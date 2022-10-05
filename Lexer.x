{
  -- Lexer first draft
  -- Mateus Almeida, Keiko Tiago, 2021
  module Lexer where
}

%wrapper "basic"
$white = [\ \t\n\r\"]
$alpha = [_a-zA-Z]
$digit = [0-9]

tokens :-
$white+				; --espaços brancos
"//".*				; -- single line comment
"/*".*"*/"		; -- multi line comment (\/\*[^\*]*\\*+(?:[^\*][^*]*\\*+)*\/)
if						{ \_ -> IF}
break				{ \_ -> BREAK}
while					{ \_ -> WHILE}
do					{ \_ -> DO}
then					{\_ -> THEN}
else					{\_ -> ELSE}
for					{\_ -> FOR}
function			{\_ -> FUNCTION}
in 					{\_ -> IN}
let 					{\_ -> LET}
of						{\_ -> OF}
to						{\_ -> TO}
var					{\_ -> VAR}
end 					{\_ -> END}
int           {\_ -> INT }
string        {\_ -> STRING }
[\"][^\n\t\ \"\(\)]*[\"]				{\s -> CONST s}
$digit+				{\s -> NUM (read s)}
$alpha($alpha|$digit)*	{\s -> ID s}
","					{\_ -> COMMA}
";"					{\_ -> SEMICOLON}
":"					{\_ -> COLON}
"+"					{\_ -> PLUS}
"-"					{\_ -> MINUS}
"*"					{\_ -> MULT}
"/"					{\_ -> DIV}
"%"					{\_ -> MOD}
"("					{\_ -> LPAREN}
")"					{\_ -> RPAREN}
"["					{\_ -> LBRACK}
"]"					{\_ -> RBRACK}
"<"					{\_ -> MINOR}
">"					{\_ -> MAJOR}
"&"					{\_ -> BITAND}
"|"						{\_ -> BITOR}
"="					{\_ -> EQUAL}
":="					{\_ -> ASSIGN}
"<="					{\_ -> MINOREQ}
">="					{\_ -> MAJOREQ}
"<>"					{\_ -> NEQ}

{
data Token =
			 ID String
			 | CONST String
			 | NUM Int
       | STRING
       | INT
       | BREAK
			 | DO
			 | IF
			 | THEN
			 | ELSE
			 | WHILE
			 | FOR
			 | FUNCTION
			 | IN
			 | LET
			 | OF
			 | TO
			 | VAR
			 | END
			 | COMMA
			 | COLON
			 | SEMICOLON
			 | LPAREN
			 | RPAREN
			 | LBRACK
			 | RBRACK
			 | PLUS
			 | MINUS
			 | MULT
			 | DIV
			 | MOD
			 | EQUAL
			 | ASSIGN
			 | MAJOR
			 | MINOR
			 | MAJOREQ
			 | MINOREQ
			 | NEQ
			 | BITAND
			 | BITOR
			 deriving (Eq, Show)
}
