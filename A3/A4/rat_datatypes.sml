
structure DataTypes=
struct 
    datatype  prog = PROG of block
    and block  = BLOCK of decseq*cmdseq
    and decseq = DECSEQ of vardec*procdec
    and vardec = prependAll of dec*vardec
    and dec =   makeVarList1 of varlist*TOK_RATIONAL
              | makeVarList2 of varlist*TOK_INTEGER
              | makeVarList3 of varlist*TOK_BOOLEAN

    and varlist = prepend1 of TOK_ID7*varlist
                
    and procdec = prepend2 of procdef*procdec
    
              
    and procdef = PROCDEF of TOK_ID6*block

    and cmdseq = LCMD of  lcmd  

    and lcmd = prepend3 of  cmd*lcmd
               
    and cmd = ACMD of aCmd 
             |CCMD of cCmd 
             |RCMD of rCmd 
             |COCMD of coCmd 
             |WCMD of wCmd 
             |PCMD of pCmd
    
    and aCmd = set  of TOK_ID5*expr
    and cCmd = call of TOK_ID4
    and rCmd = RD  of TOK_ID3
    and pCmd = printexpr of expr
    and coCmd = ITE of bexpr*cmdseq*cmdseq
    and wCmd = WH of bexpr*cmdseq

    and expr = REXPR of  rexpr
              | IEXPR of iexpr  
              | BEXPR of bexpr 
              | REF of TOK_ID
    
    and rexpr = NEGR of rexpr
               | ADDR of rexpr*rexpr
               | SUBR of rexpr*rexpr
               | MULR of rexpr*rexpr
               | DIVR of rexpr*rexpr
               | TOK_RAT

    and iexpr =  TOK_NUM 
               
               | NEG of TOK_NUM
               | ADD of iexpr*iexpr
               | SUB of iexpr*iexpr
               | MUL of iexpr*iexpr
               | DIV of iexpr*iexpr
               | MOD of iexpr*iexpr
               
    
    and bexpr =  TRUE
               | FALSE
               | NOT of bexpr
               | AND of bexpr*bexpr 
               | OR of bexpr*bexpr 
               | NE of iexpr*iexpr  
               | EQ of iexpr*iexpr 
               | LT of iexpr*iexpr 
               | LE of iexpr*iexpr  
               | GT of iexpr*iexpr 
               | GE of iexpr*iexpr 
               | NER of rexpr*rexpr  
               | EQR of rexpr*rexpr 
               | LTR of rexpr*rexpr 
               | LER of rexpr*rexpr  
               | GTR of rexpr*rexpr 
               | GER of rexpr*rexpr 
               
               
    and TOK_ID = id  of string
    
    and TOK_ID3 = id3  of string
    and TOK_ID4 = id4  of string
    and TOK_ID5 = id5  of string
    and TOK_ID6 = id6  of string
    and TOK_ID7 = id7  of string
    and TOK_ID8 = id8  of string
    and TOK_ID9 = id9  of string
    and TOK_ID11 = id11  of string
    and TOK_RATIONAL = rational of string
    and TOK_INTEGER = integer of string
    and TOK_BOOLEAN = boolean of string
    and TOK_PROCEDURE = procedure of string
    and RAT = RATional of string
    and NUM = num of string
    and TOK_RAT = rat2 of string
    and TOK_NUM = Num of string

    exception SemanticError;
end;