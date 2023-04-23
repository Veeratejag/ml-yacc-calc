use "rational.sml";

structure Rational = Rational(BigInt);

fun evali expr = 
    case expr of 
         ADD(expr1,expr2) => BigInt.Add(evali(expr1),evali(expr2))
       | SUB (expr1,expr2)=> BigInt.Sub(evali(expr1),evali(expr2))
       | MUL(expr1,expr2)=> BigInt.Mul(evali(expr1),evali(expr2))
       | DIV (expr1,expr2)=> BigInt.Div(evali(expr1),evali(expr2))
       | MOD(expr1,expr2) => BigInt.Mod(evali(expr1),evali(expr2))
       | NEG(expr) => BigInt.Neg(evali(expr))
       | TOK_NUM:BigInt => TOK_NUM;
       
fun evalr expr = 
    case expr of 
         ADDR(expr1,expr2) => Rational.add(evalr(expr1),evalr(expr2))
       | SUBR (expr1,expr2)=> Rational.subtract(evalr(expr1),evalr(expr2))
       | MULR (expr1,expr2)=> Rational.multiply(evalr(expr1),evalr(expr2))
       | DIVR (expr1,expr2)=> Rational.divide(evalr(expr1),evalr(expr2))
       | MAKERAT(expr1,expr1) => valOf(Rational.make_rat(evali(expr1),evali(expr2)))
       | INVERSE(expr) => valOf(Rational.inverse(evalr(expr)))
       | FROMINT(expr) => Rational.rat(evali(expr))
       | NEGR(expr) => Rational.neg(evalr(expr))
       | RAT(TOK_RAT:Rational) => TOK_RAT;
fun evalb expr = 
    case expr of 
       | TRUE => true
       | FALSE => false
       | AND(expr1,expr2)=> evalb(expr1) andalso evalb(expr2)
       | OR(expr1,expr2) => evalb(expr1) orelse evalb(expr2)
       | NOT(expr) => not(evalb(expr))
       | GT (iexpr1,iexpr2) => BigInt.GT(evali(iexpr1),evali(iexpr2))
       | GE (iexpr1,iexpr2)=> BigInt.GE(evali(iexpr1),evali(iexpr2))
       | LT (iexpr1,iexpr2)=> BigInt.LT(evali(iexpr1),evali(iexpr2))
       | LE (iexpr1,iexpr2)=> BigInt.LE(evali(iexpr1),evali(iexpr2))
       | EQ(iexpr1,iexpr2)=> BigInt.Equal(evali(iexpr1),evali(iexpr2))
       | NE(iexpr1,iexpr2)=> BigInt.NE(evali(iexpr1),evali(iexpr2))
       | GTR(rexpr1,rexpr2) => Rational.GTR(evalr(rexpr1),evalr(rexpr2))
       | GER(rexpr1,rexpr2) => Rational.GER(evalr(rexpr1),evalr(rexpr2))
       | LTR(rexpr1,rexpr2) => Rational.less(evalr(rexpr1),evalr(rexpr2))
       | LER(rexpr1,rexpr2) => Rational.LER(evalr(rexpr1),evalr(rexpr2))
       | EQR(rexpr1,rexpr2) => Rational.equal(evalr(rexpr1),evalr(rexpr2))
       | NER(rexpr1,rexpr2) => Rational.NER(evalr(rexpr1),evalr(rexpr2));
