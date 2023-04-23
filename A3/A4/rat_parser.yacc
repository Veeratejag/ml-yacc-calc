open DataTypes
open Dictionary

val SymbolDict = ref (create())

fun makeVarList (l::L) t = (
        SymbolDict := update (!SymbolDict) l (t l);
        (t l)::(makeVarList L t))
  | makeVarList []     t = []

fun prependAll (l::L) L' = l::(prependAll L L')
  | prependAll []     L' = L'

fun set id expr =
    case lookup (!SymbolDict) id of 
        (INTEGER s) => SETINT (id, expr)
      | (BOOLEAN b) => SETBOOL (id, expr)
      | (RATIONAL r) => SETRATIONAL (id, expr);
fun printexpr expr = 
    case lookup (!SymbolDict) expr of 
        (INTEGER s) => print((BigInt.toString expr)^"\n")
      | (BOOLEAN b) => if expr then print("tt\n") else print("ff\n")
      | (RATIONAL r) =>print((Rational.showDecimal expr)^"\n");

%%
%name Rational

%term TOK_ADD | TOK_UMINUS | TOK_SUB | TOK_MUL | TOK_DIV | TOK_MOD | 
      TOK_ADDR | TOK_SUBR | TOK_MULR | TOK_DIVR | TOK_EQ | 
      TOK_NE | TOK_GT | TOK_GE | TOK_LT | TOK_LE | TOK_AND | TOK_OR | TOK_NOT | 
      TOK_ASSIGN | TOK_PROCEDURE | TOK_VAR | TOK_INTEGER | TOK_BOOLEAN | TOK_READ | 
      TOK_CALL | TOK_PRINT | TOK_RATIONAL | 
      TOK_WRITE | TOK_IF | TOK_THEN | TOK_ELSE | TOK_FI | TOK_WHILE | TOK_DO | 
      TOK_OD | TOK_TT | TOK_FF | TOK_SEMICOLON | 
      TOK_COMMA | TOK_LBRACE | TOK_RBRACE | TOK_LPAREN | TOK_RPAREN | 
      TOK_ID of string | TOK_NUM of BigInt | TOK_EOF | TOK_RAT of RATIONAL |
      TOK_MAKERAT | TOK_FROMINT | TOK_FROMDECIMAL | TOK_INVERSE | TOK_SHOWDECIMAL | TOK_TODECIMAL | TOK_SHOWRAT


%nonterm prog of PROG | dec of DEC list | varlist of string list |
         vardec of  VARDEC | cmdseq of CMD list |  cmd of CMD |
         expr of EXPR | block of BLOCK | iexpr of INTEXPR | bexpr of BOOLEXPR | rexpr of RATEXPR | 
         ident of IDENT| aCmd of ACMD| cCmd of CCMD| rCmd of RCMD| pCmd of PCMD |
         coCmd of COCMD | wCmd of WCMD | 
         decseq of DECSEQ | procdec of PROCDEC list| ratdec of RATDEC list| intdec of INTDEC list|
         booldec of BOOLDEC list| procdef of PROCDEF |
         lident of IDENT list | lcmd of CMD list 

%left TOK_OR
%left TOK_AND
%right TOK_NOT
%left TOK_EQ TOK_NE TOK_LE TOK_LT TOK_GE TOK_GT
%left TOK_ADD TOK_SUB TOK_ADDR TOK_SUBR
%left TOK_MUL TOK_DIV TOK_MOD TOK_MULR TOK_DIVR

%pos int
%eop TOK_EOF
%noshift TOK_EOF
%nodefault
%verbose

%keyword TOK_PROCEDURE TOK_VAR TOK_INTEGER TOK_RATIONAL TOK_BOOLEAN 
        TOK_MAKERAT TOK_FROMINT TOK_FROMDECIMAL TOK_INVERSE TOK_SHOWDECIMAL TOK_TODECIMAL TOK_SHOWRAT 
        TOK_READ TOK_CALL TOK_PRINT 
        TOK_IF TOK_THEN TOK_ELSE TOK_FI 
        TOK_WHILE TOK_DO TOK_OD 
        TOK_TT TOK_FF  
%arg (fileName) : string
%%

prog : block  (PROG(block))
block : decseq cmdseq  (BLOCK(decseq,cmdseq))
decseq : vardec procdec (DECSEQ(vardec, procdec))

vardec: dec TOK_SEMICOLON vardec (prependAll dec vardec)
       |             ([])
dec : TOK_RATIONAL varlist TOK_SEMICOLON (makeVarList varlist TOK_RATIONAL)

    | TOK_INTEGER varlist TOK_SEMICOLON (makeVarList varlist TOK_INTEGER)

    | TOK_BOOLEAN varlist  TOK_SEMICOLON (makeVarList varlist TOK_BOOLEAN)

varlist : TOK_ID TOK_COMMA varlist (TOK_ID::varlist)
       | TOK_ID               ([TOK_ID])

procdec :procdef  TOK_SEMICOLON procdec (procdef :: procdec)
       |  ([])

procdef : TOK_PROCEDURE TOK_ID block  (PROCDEF(TOK_ID,block))

cmdseq : TOK_LBRACE lcmd TOK_RBRACE  (lcmd)

lcmd : cmd TOK_SEMICOLON lcmd (cmd::lcmd)
    |   ([])

cmd : aCmd (ACMD(aCmd))
    | cCmd (CCMD(cCmd))
    | rCmd (RCMD(rCmd))
    | pCmd (PCMD(pCmd))
    | coCmd (COCMD(coCmd))
    | wCmd (WCMD(wCmd))

aCmd : TOK_ID TOK_ASSIGN expr (set TOK_ID expr)

cCmd : TOK_CALL TOK_ID (call TOK_ID)

rCmd : TOK_READ TOK_LPAREN TOK_ID TOK_RPAREN (RD TOK_ID)

pCmd : TOK_PRINT TOK_LPAREN expr TOK_RPAREN (printexpr expr)
      

coCmd : TOK_IF bexpr TOK_THEN cmdseq TOK_ELSE cmdseq TOK_FI (ITE (bexpr,cmdseq1,cmdseq2))

wCmd : TOK_WHILE bexpr TOK_DO cmdseq TOK_OD (WH (bexpr,cmdseq))
expr: rexpr  (REXPR(rexpr))
    | iexpr  (IEXPR(iexpr))
    | bexpr  (BEXPR(bexpr))
    | TOK_ID (case lookup (!SymbolTable) TOK_ID of (INT s) => (IREF TOK_ID) | (BOOL b) => (BREF TOK_ID) | (RAT r)=>(RREF TOK_ID))



rexpr : rexpr TOK_ADDR rexpr %prec TOK_ADDR (ADDR(rexpr1,rexpr2))
      | rexpr TOK_SUBR rexpr %prec TOK_SUBR (SUBR(rexpr1,rexpr2))
      | rexpr TOK_MULR rexpr %prec TOK_MULR (MULR(rexpr1,rexpr2))
      | rexpr TOK_DIVR rexpr %prec TOK_DIVR (DIVR(rexpr1,rexpr2))
      | TOK_UMINUS TOK_RAT (NEGR(TOK_RAT))
      | TOK_RAT (TOK_RAT)
      


iexpr : iexpr TOK_ADD iexpr %prec TOK_ADD (ADD ( iexpr1, iexpr2))
    | iexpr TOK_SUB iexpr %prec TOK_SUB (SUB( iexpr1, iexpr2))
    | iexpr TOK_MUL iexpr %prec TOK_MUL (MUL( iexpr1, iexpr2))
    | iexpr TOK_DIV iexpr %prec TOK_DIV (DIV( iexpr1, iexpr2))
    | iexpr TOK_MOD iexpr %prec TOK_MOD (MOD( iexpr1, iexpr2))
    | TOK_UMINUS TOK_NUM (NEG(TOK_NUM))
    | TOK_NUM (TOK_NUM)
    

bexpr : iexpr TOK_GT iexpr %prec TOK_GT (GT (iexpr1,iexpr2))
      | iexpr TOK_GE iexpr %prec TOK_GE (GE (iexpr1,iexpr2))
      | iexpr TOK_LT iexpr %prec TOK_LT (LT (iexpr1,iexpr2))
      | iexpr TOK_LE iexpr %prec TOK_LE (LE (iexpr1,iexpr2))
      | iexpr TOK_EQ iexpr %prec TOK_EQ (EQ(iexpr1,iexpr2))
      | iexpr TOK_NE iexpr %prec TOK_NE (NE(iexpr1,iexpr2))
      | rexpr TOK_GT rexpr %prec TOK_GT (GTR(rexpr1,rexpr2))
      | rexpr TOK_GE rexpr %prec TOK_GE (GER(rexpr1,rexpr2))
      | rexpr TOK_LT rexpr %prec TOK_LT (LTR(rexpr1,rexpr2))
      | rexpr TOK_LE rexpr %prec TOK_LE (LER(rexpr1,rexpr2))
      | rexpr TOK_EQ rexpr %prec TOK_EQ (EQR(rexpr1,rexpr2))
      | rexpr TOK_NE rexpr %prec TOK_NE (NER(rexpr1,rexpr2))
      | bexpr TOK_OR bexpr %prec TOK_OR (OR(bexpr1,bexpr2))
      | bexpr TOK_AND bexpr %prec TOK_AND (AND(bexpr1, bexpr2))
      | TOK_NOT bexpr %prec TOK_NOT (NOT(bexpr))
      | TOK_TT (TRUE)
      | TOK_FF (FALSE)
      




