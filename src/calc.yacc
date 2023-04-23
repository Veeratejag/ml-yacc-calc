(* An integer exponentiation function *) 
fun expt (base,0) = 1
| expt(base,power) = base*(expt(base,power-1))
%%
(* Only changed parts shown *)
%nonterm Start of int
| Exp of int
| Term of int | Factor of int | Unit of int

%%
Start: Exp (Exp)
Exp : Term (Term)
(* The following rules specify left associativity *) | Exp ADD Term (Exp + Term)
| Exp SUB Term (Exp - Term)
Term : Factor (Factor)
(* The following rules specify left associativity *) | Term MUL Factor (Term * Factor)
| Term DIV Factor (Term div Factor)
(* div is integer division *)
Factor : Unit (Unit)
(* The following rule specifies right associativity *)
| Unit EXPT Factor (expt(Unit,Factor))
Unit : INT (INT)
| LPAREN Exp RPAREN (Exp)

