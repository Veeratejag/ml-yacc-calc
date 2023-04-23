open Tokens

type pos = int

(* svalue stands for syntatic value *)
type lexresult = (svalue, pos) token

fun eof () = Tokens.EOF(0, 0)

fun integer(str, lexPos) =
  case Int.fromString(str) of
       NONE => raise Fail("Shouldn't happen: sequence of digits" ^
                          "not recognized as integer--" ^ str)
     | SOME(n) => INT(n, lexPos, lexPos)
  
fun init() = ()

%%

%header (functor IntexpLexFun(structure Tokens: Intexp_TOKENS));

alpha = [a-zA-Z];
digit = [0-9];

whitespace = [\ \t\n];
symbol = [+*^()\[]];
any = [^];

%%

{digit}+ => (integer(yytext, yypos));
{whitespace} => (lex());
"+" => (ADD(yypos, yypos));
"-" => (SUB(yypos, yypos));
"*" => (MUL(yypos, yypos));
"/" => (DIV(yypos, yypos));
"^" => (EXPT(yypos, yypos));
"(" => (LPAREN(yypos, yypos));
")" => (RPAREN(yypos, yypos));
