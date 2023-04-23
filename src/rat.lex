open Tokens
use "rational1.sml";
struct Rational = Rational(BigInt);

type pos = int

type lexresult = (svalue, pos) token

fun eof () = Tokens.EOF(0, 0)


%%

alpha = [a-zA-Z];
digit = [0-9];
rat = {digit}+"."{digit}*"("{digit}+")";
whitespace = [\ \t\n];


%%

rat => (Rational.fromDecimal(String.substring(yytext,0, yypos)));
{whitespace} => (lex());
