structure DataTypes =
    

    struct
    datatype PROG = BLOCK
    datatype DEC = VARDEC | PROCDEC
    datatype VARDEC = RATDEC list * INTDEC list * BOOLDEC list
    datatype CMD = ACMD | CCMD | RCMD | PCMD | COCMD | WCMD
    datatype EXPR = INTEXPR | BOOLEXPR | RATEXPR
    datatype BLOCK = DECSEQ * CMD list
    datatype DECSEQ = VARDEC * PROCDEC list
    datatype PROCDEC = PROCDEF list
    datatype RATDEC = string * Rational
    datatype INTDEC = string * int
    datatype BOOLDEC = string * bool
    datatype PROCDEF = string * BLOCK
    datatype IDENT = string
    datatype ACMD = SETINT of (IDENT * INTEXPR) | SETBOOL of (IDENT * BOOLEXPR) | SETRATIONAL of (IDENT * RATEXPR)
    datatype CCMD = CALL of IDENT
    datatype RCMD = RD of IDENT
    datatype PCMD = PRINT of EXPR
    datatype COCMD = ITE of (BOOLEXPR * CMD list * CMD list)
    datatype WCMD = WH of (BOOLEXPR * CMD list)

    datatype Rational = RAT of {numer : int, denom : int}
end
