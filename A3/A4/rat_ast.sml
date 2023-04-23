structure AST = struct
  datatype iexp = Int of int 
               | BinApp of binop * iexp * iexp

  and binop = Add | Sub | Mul | Div | Mod
  and bexp = Bool of bool 
            | BinAppB of binopbool*bexp*bexp
            | UniAppB of uniop*bexp
  and binopbool = And | Or
  and uniop = Not
  and rexp = Rat of rat 
            | BinAppR of binoprat*rexp*rexp
            | UniAppR of unioprat*rexp
  and binoprat = Addr | Subr | Mulr | Divr
  and unioprat = Inverse 
  and rat =string

end