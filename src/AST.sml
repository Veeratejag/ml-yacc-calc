structure AST = struct
  datatype exp = Int of int
               | BinApp of binop * exp * exp

  and binop = Add | Sub | Mul | Div | Expt
end
