open DataTypes
open Dictionary

val SymbolTable = ref (create())

fun makeVarList (l::L) t = (
        SymbolTable := update (!SymbolTable) l (t l);
        (t l)::(makeVarList L t))
  | makeVarList []     t = []

fun prependAll (l::L) L' = l::(prependAll L L')
  | prependAll []     L' = L'

fun set id expr =
    case lookup (!SymbolTable) id of 
        (INT s) => SETINT (id, expr)
      | (BOOL b) => SETBOOL (id, expr)
fun 

%%
%name While

%term TOK_ADD | TOK_UMINUS | TOK_SUB | TOK_MUL | TOK_DIV | TOK_MOD | TOK_EQ | 
      TOK_NE | TOK_GT | TOK_GE | TOK_LT | TOK_LE | TOK_AND | TOK_OR | TOK_NOT | 
      TOK_ASSIGN | TOK_PROGRAM | TOK_VAR | TOK_INT | TOK_BOOL | TOK_READ | 
      TOK_WRITE | TOK_IF | TOK_THEN | TOK_ELSE | TOK_ENDIF | TOK_WHILE | TOK_DO | 
      TOK_ENDWH | TOK_TT | TOK_FF | TOK_BLOCKSTART | TOK_COLON | TOK_SEMICOLON | 
      TOK_COMMA | TOK_LBRACE | TOK_RBRACE | TOK_LPAREN | TOK_RPAREN | 
      TOK_ID of string | TOK_NUM of int | TOK_EOF

%nonterm prog of PROG | declist of DEC list | dec of DEC list | 
         varlist of string list | cmdlist of CMD list | cmd of CMD | 
         cmdseq of CMD list | expr of EXPR

%left TOK_OR
%left TOK_AND
%right TOK_NOT
%left TOK_EQ TOK_NE TOK_LE TOK_LT TOK_GE TOK_GT
%left TOK_ADD TOK_SUB
%left TOK_MUL TOK_DIV TOK_MOD

%pos int
%eop TOK_EOF
%noshift TOK_EOF
%nodefault
%verbose
%keyword TOK_PROGRAM TOK_VAR TOK_INT TOK_BOOL TOK_READ TOK_WRITE TOK_IF TOK_THEN 
         TOK_ELSE TOK_ENDIF TOK_WHILE TOK_DO TOK_ENDWH TOK_TT TOK_FF
%arg (fileName) : string
%%
prog: TOK_PROGRAM TOK_ID TOK_BLOCKSTART declist cmdseq (PROG (declist, cmdseq))

declist: dec TOK_SEMICOLON declist (prependAll dec declist)
       |             ([])
dec: TOK_VAR varlist TOK_COLON TOK_INT  (makeVarList varlist INT)
   | TOK_VAR varlist TOK_COLON TOK_BOOL (makeVarList varlist BOOL)
varlist: TOK_ID TOK_COMMA varlist (TOK_ID::varlist)
       | TOK_ID               ([TOK_ID])

cmdseq: TOK_LBRACE cmdlist TOK_RBRACE (cmdlist)
cmdlist: cmd TOK_SEMICOLON cmdlist (cmd::cmdlist)
       |                       ([])

cmd: TOK_READ TOK_ID (RD TOK_ID)
   | TOK_WRITE expr (WR expr)
   | TOK_ID TOK_ASSIGN expr (set TOK_ID expr)
   | TOK_IF expr TOK_THEN cmdseq TOK_ELSE cmdseq TOK_ENDIF (ITE (expr,cmdseq1,cmdseq2))
   | TOK_WHILE expr TOK_DO cmdseq TOK_ENDWH (WH (expr,cmdseq))

expr: expr TOK_ADD expr %prec TOK_ADD (ADD (expr1,expr2))
    | expr TOK_SUB expr %prec TOK_SUB (SUB (expr1,expr2))
    | expr TOK_MUL expr %prec TOK_MUL (MUL (expr1,expr2))
    | expr TOK_DIV expr %prec TOK_DIV (DIV (expr1,expr2))
    | expr TOK_MOD expr %prec TOK_MOD (MOD (expr1,expr2))
    | TOK_LPAREN expr TOK_RPAREN (expr)
    | TOK_ID (case lookup (!SymbolTable) TOK_ID of (INT s) => (IREF TOK_ID) | (BOOL b) => (BREF TOK_ID))
    | TOK_NUM (NUM TOK_NUM)
    | TOK_UMINUS TOK_NUM (NUM (~1*TOK_NUM))
    | TOK_ADD TOK_NUM (NUM TOK_NUM)
    | expr TOK_OR expr %prec TOK_OR (OR (expr1, expr2))
    | expr TOK_AND expr %prec TOK_AND (AND (expr1, expr2))
    | TOK_NOT expr %prec TOK_NOT (NOT expr)
    | expr TOK_GT expr %prec TOK_GT (GT (expr1, expr2))
    | expr TOK_GE expr %prec TOK_GE (GE (expr1, expr2))
    | expr TOK_LT expr %prec TOK_LT (LT (expr1, expr2))
    | expr TOK_LE expr %prec TOK_LE (LE (expr1, expr2))
    | expr TOK_EQ expr %prec TOK_EQ (EQ (expr1, expr2))
    | expr TOK_NE expr %prec TOK_NE (NE (expr1, expr2))
    | TOK_TT (TRUE)
    | TOK_FF (FALSE)