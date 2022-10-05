{
	-- Parser first draft
	-- Mateus Almeida, Keiko Tiago, 2021
module Parser where
import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token

string			{ STRING }
int    			{INT}
num				{ NUM $$}
id					{ ID $$}
'+'					{PLUS}
'-'					{MINUS}
'*'					{MULT}
'/'					{DIV}
'%'					{MOD}
':='				{ASSIGN}
'('					{LPAREN}
')'					{RPAREN}
'['					{LBRACK}
']'					{RBRACK}
'='					{EQUAL}
'<>'				{NEQ}
'<='				{MINOREQ}
'>='				{MAJOREQ}
'<'          {MINOR}
'>'          {MAJOR}
','					{COMMA}
';'					{SEMICOLON}
':'					{COLON}
'&'					{BITAND}
'|'					{BITOR}
if					{IF}
then				{THEN}
else				{ELSE}
while				{WHILE}
do				{DO}
break			{BREAK}
end				{END}
for				{FOR}
function		{FUNCTION}
in					{IN}
let 				{LET}
of 				{OF}
to         {TO}
var				{VAR}

--precendências

%nonassoc '&' '|' ":="
%nonassoc '=' "<>" '>' '<' ">=" "<="
%left '-' '+'
%left '*' '/' '%'

--gramáticas
%%
Prog : let DeclL in ExpS  {LetIn $2 $4}

Exp : id ':=' Exp          {Assign $1 $3}
    | if Exp then Exp          {IfThen $2 $4}
    | if Exp then Exp else Exp {IfThenElse $2 $4 $6}
    | while Exp do Exp         {WhileDo $2 $4}
    | for id ':=' Exp to Exp do Exp {For $2 $4 $6 $8}
    | break                     {Break}
    | let VarDeclL in ExpS end  {LetInEnd $2 $4}
	| Exp '=' Exp			{Cond Equal $1 $3}
    | Exp '<>'	Exp			{Cond NotEq $1 $3}
    | Exp '<='	Exp			{Cond LessEq $1 $3}
    | Exp '>=' Exp         {Cond GreatEq $1 $3}
    | Exp '>' Exp          {Cond Greater $1 $3}
    | Exp '<' Exp          {Cond Lesser $1 $3}
    | Exp '&' Exp  {And $1 $3}
    | Exp '|' Exp  {Or $1 $3}
	| ArithExp     {$1}

ArithExp : num                      { Num $1}
	     | id						{Var $1}
		 | id '(' ExpL ')'          {CallFunct $1 $3}
		 | '(' ExpS ')'             {Block $2}
         | ArithExp '+' ArithExp    {Op Add $1 $3 }
         | ArithExp '-' ArithExp    {Op Sub $1 $3 }
         | ArithExp '*' ArithExp    {Op Mult $1 $3 }
         | ArithExp '/' ArithExp    {Op Div $1 $3 }
         | ArithExp '%' ArithExp    {Op Mod $1 $3 }

ExpS :  Exp ExpO    {$1 : $2}
     |               {[]}

ExpL : Exp ExpT     {$1 : $2}
     |                  {[]}

ExpO : ';' Exp ExpO {$2 : $3}
     |              {[]}

ExpT : ',' Exp ExpT {$2 : $3}
     |              {[]}


VarDecl : var id ':=' Exp     {Declare $2 $4}

VarDeclL : VarDecl VarDeclL   {$1 : $2}
         |          {[]}

DeclL : Decl DeclL      {$1 :$2}
      |                 {[]}

Decl : VarDecl    {DeclVar $1}
      | Funct  {DeclFunct $1}

Funct : function id '(' TypeFieldL ')' '=' Exp      {Procedure $2 $4 $7}
      | function id '(' TypeFieldL ')' ':' TypeId '=' Exp {Function $2 $4 $7 $9}

TypeFieldL : TypeField                {[$1]}
         | TypeField ',' TypeFieldL   {$1 : $3}

TypeField : id ':' TypeId {Field $1 $3}

TypeId : int          {TypeInt}
       | string       {TypeString}


{
data Prog = LetIn [Decl] [Exp]
        deriving Show

data Exp = Num Int
				 | Var String
         | Op BinOp Exp Exp
         | WhileDo Exp Exp
         | IfThen Exp Exp
         | IfThenElse Exp Exp Exp
         | For String Exp Exp Exp
         | Block [Exp]
         | CallFunct String [Exp]
         | Assign String Exp
         | LetInEnd [VarDecl] [Exp]
         | Break
		 | Cond RelOp Exp Exp
		 | And Exp Exp
		 | Or Exp Exp
         deriving Show

data BinOp = Add| Sub| Mult | Div | Mod
		    deriving Show

data RelOp= Equal | NotEq | GreatEq | LessEq | Lesser | Greater
        deriving Show

data Decl = DeclVar VarDecl
          | DeclFunct Funct
          deriving Show

data VarDecl = Declare String Exp
          deriving Show

data Funct = Procedure String [TypeField] Exp
          | Function String [TypeField] TypeId Exp
          deriving Show

data TypeId = TypeInt | TypeString
         deriving Show

data TypeField = Field String TypeId
         deriving Show


parseError :: [Token] -> a
parseError toks = error ("parse error at "++ show toks)
}
