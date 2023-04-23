signature Rational_TOKENS =
sig
type ('a,'b) token
type svalue
val TOK_SHOWRAT:  'a * 'a -> (svalue,'a) token
val TOK_TODECIMAL:  'a * 'a -> (svalue,'a) token
val TOK_SHOWDECIMAL:  'a * 'a -> (svalue,'a) token
val TOK_INVERSE:  'a * 'a -> (svalue,'a) token
val TOK_FROMDECIMAL:  'a * 'a -> (svalue,'a) token
val TOK_FROMINT:  'a * 'a -> (svalue,'a) token
val TOK_MAKERAT:  'a * 'a -> (svalue,'a) token
val TOK_RAT: (RATIONAL) *  'a * 'a -> (svalue,'a) token
val TOK_EOF:  'a * 'a -> (svalue,'a) token
val TOK_NUM: (BigInt) *  'a * 'a -> (svalue,'a) token
val TOK_ID: (string) *  'a * 'a -> (svalue,'a) token
val TOK_RPAREN:  'a * 'a -> (svalue,'a) token
val TOK_LPAREN:  'a * 'a -> (svalue,'a) token
val TOK_RBRACE:  'a * 'a -> (svalue,'a) token
val TOK_LBRACE:  'a * 'a -> (svalue,'a) token
val TOK_COMMA:  'a * 'a -> (svalue,'a) token
val TOK_SEMICOLON:  'a * 'a -> (svalue,'a) token
val TOK_FF:  'a * 'a -> (svalue,'a) token
val TOK_TT:  'a * 'a -> (svalue,'a) token
val TOK_OD:  'a * 'a -> (svalue,'a) token
val TOK_DO:  'a * 'a -> (svalue,'a) token
val TOK_WHILE:  'a * 'a -> (svalue,'a) token
val TOK_FI:  'a * 'a -> (svalue,'a) token
val TOK_ELSE:  'a * 'a -> (svalue,'a) token
val TOK_THEN:  'a * 'a -> (svalue,'a) token
val TOK_IF:  'a * 'a -> (svalue,'a) token
val TOK_WRITE:  'a * 'a -> (svalue,'a) token
val TOK_RATIONAL:  'a * 'a -> (svalue,'a) token
val TOK_PRINT:  'a * 'a -> (svalue,'a) token
val TOK_CALL:  'a * 'a -> (svalue,'a) token
val TOK_READ:  'a * 'a -> (svalue,'a) token
val TOK_BOOLEAN:  'a * 'a -> (svalue,'a) token
val TOK_INTEGER:  'a * 'a -> (svalue,'a) token
val TOK_VAR:  'a * 'a -> (svalue,'a) token
val TOK_PROCEDURE:  'a * 'a -> (svalue,'a) token
val TOK_ASSIGN:  'a * 'a -> (svalue,'a) token
val TOK_NOT:  'a * 'a -> (svalue,'a) token
val TOK_OR:  'a * 'a -> (svalue,'a) token
val TOK_AND:  'a * 'a -> (svalue,'a) token
val TOK_LE:  'a * 'a -> (svalue,'a) token
val TOK_LT:  'a * 'a -> (svalue,'a) token
val TOK_GE:  'a * 'a -> (svalue,'a) token
val TOK_GT:  'a * 'a -> (svalue,'a) token
val TOK_NE:  'a * 'a -> (svalue,'a) token
val TOK_EQ:  'a * 'a -> (svalue,'a) token
val TOK_DIVR:  'a * 'a -> (svalue,'a) token
val TOK_MULR:  'a * 'a -> (svalue,'a) token
val TOK_SUBR:  'a * 'a -> (svalue,'a) token
val TOK_ADDR:  'a * 'a -> (svalue,'a) token
val TOK_MOD:  'a * 'a -> (svalue,'a) token
val TOK_DIV:  'a * 'a -> (svalue,'a) token
val TOK_MUL:  'a * 'a -> (svalue,'a) token
val TOK_SUB:  'a * 'a -> (svalue,'a) token
val TOK_UMINUS:  'a * 'a -> (svalue,'a) token
val TOK_ADD:  'a * 'a -> (svalue,'a) token
end
signature Rational_LRVALS=
sig
structure Tokens : Rational_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
