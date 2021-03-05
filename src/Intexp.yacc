(* *)

%%

%name Intexp

%pos int

%term
    INT of int
  | ADD | SUB | MUL | DIV | EXPT
  | LPAREN
  | RPAREN
  | EOF

(* This reminds me of the concept of concrete/abstract syntax *)
%nonterm
    Start of AST.exp
  | Exp of AST.exp
  | Term of AST.exp
  | Factor of AST.exp
  | Unit of AST.exp

%left ADD SUB
%left MUL DIV
%right EXPT
 
%start Start

%keyword

%eop EOF

%noshift EOF

%nodefault

%verbose

%value INT(0)

%%

(* These are the reduction rules *)
Start: Exp (Exp)

Exp: Term (Term)
  | Exp ADD Term (AST.BinApp(AST.Add, Exp, Term))
  | Exp SUB Term (AST.BinApp(AST.Sub, Exp, Term))

Term: Factor (Factor)
  | Term MUL Factor (AST.BinApp(AST.Mul, Term, Factor))
  | Term DIV Factor (AST.BinApp(AST.Div, Term, Factor))

Factor: Unit (Unit)
  | Unit EXPT Factor (AST.BinApp(AST.Expt, Unit, Factor))

Unit: INT (AST.Int(INT))
  | LPAREN Exp RPAREN (Exp)
