functor IntexpLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Intexp_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* *)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\008\000\007\000\007\000\000\000\
\\001\000\002\000\023\000\003\000\023\000\004\000\011\000\005\000\010\000\
\\008\000\023\000\009\000\023\000\000\000\
\\001\000\002\000\024\000\003\000\024\000\004\000\011\000\005\000\010\000\
\\008\000\024\000\009\000\024\000\000\000\
\\001\000\002\000\025\000\003\000\025\000\004\000\011\000\005\000\010\000\
\\008\000\025\000\009\000\025\000\000\000\
\\001\000\002\000\026\000\003\000\026\000\004\000\026\000\005\000\026\000\
\\008\000\026\000\009\000\026\000\000\000\
\\001\000\002\000\027\000\003\000\027\000\004\000\027\000\005\000\027\000\
\\008\000\027\000\009\000\027\000\000\000\
\\001\000\002\000\028\000\003\000\028\000\004\000\028\000\005\000\028\000\
\\008\000\028\000\009\000\028\000\000\000\
\\001\000\002\000\029\000\003\000\029\000\004\000\029\000\005\000\029\000\
\\006\000\009\000\008\000\029\000\009\000\029\000\000\000\
\\001\000\002\000\030\000\003\000\030\000\004\000\030\000\005\000\030\000\
\\008\000\030\000\009\000\030\000\000\000\
\\001\000\002\000\031\000\003\000\031\000\004\000\031\000\005\000\031\000\
\\006\000\031\000\008\000\031\000\009\000\031\000\000\000\
\\001\000\002\000\032\000\003\000\032\000\004\000\032\000\005\000\032\000\
\\006\000\032\000\008\000\032\000\009\000\032\000\000\000\
\\001\000\002\000\013\000\003\000\012\000\008\000\020\000\000\000\
\\001\000\002\000\013\000\003\000\012\000\009\000\022\000\000\000\
\\001\000\009\000\000\000\000\000\
\"
val actionRowNumbers =
"\000\000\007\000\004\000\001\000\
\\012\000\000\000\009\000\000\000\
\\000\000\000\000\000\000\000\000\
\\011\000\008\000\006\000\005\000\
\\003\000\002\000\010\000\013\000"
val gotoT =
"\
\\001\000\019\000\002\000\004\000\003\000\003\000\004\000\002\000\
\\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\012\000\003\000\003\000\004\000\002\000\005\000\001\000\000\000\
\\000\000\
\\004\000\013\000\005\000\001\000\000\000\
\\004\000\014\000\005\000\001\000\000\000\
\\004\000\015\000\005\000\001\000\000\000\
\\003\000\016\000\004\000\002\000\005\000\001\000\000\000\
\\003\000\017\000\004\000\002\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 20
val numrules = 11
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | INT of unit ->  (int) | Unit of unit ->  (AST.exp)
 | Factor of unit ->  (AST.exp) | Term of unit ->  (AST.exp)
 | Exp of unit ->  (AST.exp) | Start of unit ->  (AST.exp)
end
type svalue = MlyValue.svalue
type result = AST.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 8) => true | _ => false
val showTerminal =
fn (T 0) => "INT"
  | (T 1) => "ADD"
  | (T 2) => "SUB"
  | (T 3) => "MUL"
  | (T 4) => "DIV"
  | (T 5) => "EXPT"
  | (T 6) => "LPAREN"
  | (T 7) => "RPAREN"
  | (T 8) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 0) => MlyValue.INT(fn () => (0)) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 
1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Exp Exp1, Exp1left, Exp1right)) :: rest671)
) => let val  result = MlyValue.Start (fn _ => let val  (Exp as Exp1)
 = Exp1 ()
 in (Exp)
end)
 in ( LrTable.NT 0, ( result, Exp1left, Exp1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.Term Term1, Term1left, Term1right)) :: 
rest671)) => let val  result = MlyValue.Exp (fn _ => let val  (Term
 as Term1) = Term1 ()
 in (Term)
end)
 in ( LrTable.NT 1, ( result, Term1left, Term1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.Term Term1, _, Term1right)) :: _ :: ( _, ( 
MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  (Exp as Exp1) = Exp1 ()
 val  (Term as Term1) = Term1 ()
 in (AST.BinApp(AST.Add, Exp, Term))
end)
 in ( LrTable.NT 1, ( result, Exp1left, Term1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.Term Term1, _, Term1right)) :: _ :: ( _, ( 
MlyValue.Exp Exp1, Exp1left, _)) :: rest671)) => let val  result = 
MlyValue.Exp (fn _ => let val  (Exp as Exp1) = Exp1 ()
 val  (Term as Term1) = Term1 ()
 in (AST.BinApp(AST.Sub, Exp, Term))
end)
 in ( LrTable.NT 1, ( result, Exp1left, Term1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.Factor Factor1, Factor1left, Factor1right))
 :: rest671)) => let val  result = MlyValue.Term (fn _ => let val  (
Factor as Factor1) = Factor1 ()
 in (Factor)
end)
 in ( LrTable.NT 2, ( result, Factor1left, Factor1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.Factor Factor1, _, Factor1right)) :: _ :: (
 _, ( MlyValue.Term Term1, Term1left, _)) :: rest671)) => let val  
result = MlyValue.Term (fn _ => let val  (Term as Term1) = Term1 ()
 val  (Factor as Factor1) = Factor1 ()
 in (AST.BinApp(AST.Mul, Term, Factor))
end)
 in ( LrTable.NT 2, ( result, Term1left, Factor1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.Factor Factor1, _, Factor1right)) :: _ :: (
 _, ( MlyValue.Term Term1, Term1left, _)) :: rest671)) => let val  
result = MlyValue.Term (fn _ => let val  (Term as Term1) = Term1 ()
 val  (Factor as Factor1) = Factor1 ()
 in (AST.BinApp(AST.Div, Term, Factor))
end)
 in ( LrTable.NT 2, ( result, Term1left, Factor1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.Unit Unit1, Unit1left, Unit1right)) :: 
rest671)) => let val  result = MlyValue.Factor (fn _ => let val  (Unit
 as Unit1) = Unit1 ()
 in (Unit)
end)
 in ( LrTable.NT 3, ( result, Unit1left, Unit1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.Factor Factor1, _, Factor1right)) :: _ :: (
 _, ( MlyValue.Unit Unit1, Unit1left, _)) :: rest671)) => let val  
result = MlyValue.Factor (fn _ => let val  (Unit as Unit1) = Unit1 ()
 val  (Factor as Factor1) = Factor1 ()
 in (AST.BinApp(AST.Expt, Unit, Factor))
end)
 in ( LrTable.NT 3, ( result, Unit1left, Factor1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.Unit (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (AST.Int(INT))
end)
 in ( LrTable.NT 4, ( result, INT1left, INT1right), rest671)
end
|  ( 10, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Exp Exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.Unit (fn _ => let val  (Exp as Exp1) = Exp1 ()
 in (Exp)
end)
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Intexp_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun ADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun EXPT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
end
end
