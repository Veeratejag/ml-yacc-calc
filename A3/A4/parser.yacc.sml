functor WhileLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : While_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
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


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\099\000\003\000\099\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\099\000\008\000\099\000\009\000\099\000\
\\010\000\099\000\011\000\099\000\012\000\099\000\013\000\099\000\
\\014\000\099\000\024\000\099\000\028\000\099\000\034\000\099\000\
\\039\000\099\000\000\000\
\\001\000\001\000\100\000\003\000\100\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\100\000\008\000\100\000\009\000\100\000\
\\010\000\100\000\011\000\100\000\012\000\100\000\013\000\100\000\
\\014\000\100\000\024\000\100\000\028\000\100\000\034\000\100\000\
\\039\000\100\000\000\000\
\\001\000\001\000\101\000\003\000\101\000\004\000\101\000\005\000\101\000\
\\006\000\101\000\007\000\101\000\008\000\101\000\009\000\101\000\
\\010\000\101\000\011\000\101\000\012\000\101\000\013\000\101\000\
\\014\000\101\000\024\000\101\000\028\000\101\000\034\000\101\000\
\\039\000\101\000\000\000\
\\001\000\001\000\102\000\003\000\102\000\004\000\102\000\005\000\102\000\
\\006\000\102\000\007\000\102\000\008\000\102\000\009\000\102\000\
\\010\000\102\000\011\000\102\000\012\000\102\000\013\000\102\000\
\\014\000\102\000\024\000\102\000\028\000\102\000\034\000\102\000\
\\039\000\102\000\000\000\
\\001\000\001\000\103\000\003\000\103\000\004\000\103\000\005\000\103\000\
\\006\000\103\000\007\000\103\000\008\000\103\000\009\000\103\000\
\\010\000\103\000\011\000\103\000\012\000\103\000\013\000\103\000\
\\014\000\103\000\024\000\103\000\028\000\103\000\034\000\103\000\
\\039\000\103\000\000\000\
\\001\000\001\000\104\000\003\000\104\000\004\000\104\000\005\000\104\000\
\\006\000\104\000\007\000\104\000\008\000\104\000\009\000\104\000\
\\010\000\104\000\011\000\104\000\012\000\104\000\013\000\104\000\
\\014\000\104\000\024\000\104\000\028\000\104\000\034\000\104\000\
\\039\000\104\000\000\000\
\\001\000\001\000\105\000\003\000\105\000\004\000\105\000\005\000\105\000\
\\006\000\105\000\007\000\105\000\008\000\105\000\009\000\105\000\
\\010\000\105\000\011\000\105\000\012\000\105\000\013\000\105\000\
\\014\000\105\000\024\000\105\000\028\000\105\000\034\000\105\000\
\\039\000\105\000\000\000\
\\001\000\001\000\106\000\003\000\106\000\004\000\106\000\005\000\106\000\
\\006\000\106\000\007\000\106\000\008\000\106\000\009\000\106\000\
\\010\000\106\000\011\000\106\000\012\000\106\000\013\000\106\000\
\\014\000\106\000\024\000\106\000\028\000\106\000\034\000\106\000\
\\039\000\106\000\000\000\
\\001\000\001\000\107\000\003\000\107\000\004\000\107\000\005\000\107\000\
\\006\000\107\000\007\000\107\000\008\000\107\000\009\000\107\000\
\\010\000\107\000\011\000\107\000\012\000\107\000\013\000\107\000\
\\014\000\107\000\024\000\107\000\028\000\107\000\034\000\107\000\
\\039\000\107\000\000\000\
\\001\000\001\000\108\000\003\000\108\000\004\000\108\000\005\000\108\000\
\\006\000\108\000\007\000\108\000\008\000\108\000\009\000\108\000\
\\010\000\108\000\011\000\108\000\012\000\108\000\013\000\108\000\
\\014\000\108\000\024\000\108\000\028\000\108\000\034\000\108\000\
\\039\000\108\000\000\000\
\\001\000\001\000\118\000\003\000\118\000\004\000\118\000\005\000\118\000\
\\006\000\118\000\007\000\118\000\008\000\118\000\009\000\118\000\
\\010\000\118\000\011\000\118\000\012\000\118\000\013\000\118\000\
\\014\000\118\000\024\000\118\000\028\000\118\000\034\000\118\000\
\\039\000\118\000\000\000\
\\001\000\001\000\119\000\003\000\119\000\004\000\119\000\005\000\119\000\
\\006\000\119\000\007\000\119\000\008\000\119\000\009\000\119\000\
\\010\000\119\000\011\000\119\000\012\000\119\000\013\000\119\000\
\\014\000\119\000\024\000\119\000\028\000\119\000\034\000\119\000\
\\039\000\119\000\000\000\
\\001\000\001\000\035\000\002\000\034\000\015\000\033\000\030\000\032\000\
\\031\000\031\000\038\000\030\000\040\000\029\000\041\000\028\000\000\000\
\\001\000\001\000\057\000\003\000\056\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\112\000\008\000\112\000\009\000\112\000\
\\010\000\112\000\011\000\112\000\012\000\112\000\013\000\112\000\
\\014\000\112\000\024\000\112\000\028\000\112\000\034\000\112\000\
\\039\000\112\000\000\000\
\\001\000\001\000\057\000\003\000\056\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\113\000\008\000\113\000\009\000\113\000\
\\010\000\113\000\011\000\113\000\012\000\113\000\013\000\113\000\
\\014\000\113\000\024\000\113\000\028\000\113\000\034\000\113\000\
\\039\000\113\000\000\000\
\\001\000\001\000\057\000\003\000\056\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\114\000\008\000\114\000\009\000\114\000\
\\010\000\114\000\011\000\114\000\012\000\114\000\013\000\114\000\
\\014\000\114\000\024\000\114\000\028\000\114\000\034\000\114\000\
\\039\000\114\000\000\000\
\\001\000\001\000\057\000\003\000\056\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\115\000\008\000\115\000\009\000\115\000\
\\010\000\115\000\011\000\115\000\012\000\115\000\013\000\115\000\
\\014\000\115\000\024\000\115\000\028\000\115\000\034\000\115\000\
\\039\000\115\000\000\000\
\\001\000\001\000\057\000\003\000\056\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\116\000\008\000\116\000\009\000\116\000\
\\010\000\116\000\011\000\116\000\012\000\116\000\013\000\116\000\
\\014\000\116\000\024\000\116\000\028\000\116\000\034\000\116\000\
\\039\000\116\000\000\000\
\\001\000\001\000\057\000\003\000\056\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\117\000\008\000\117\000\009\000\117\000\
\\010\000\117\000\011\000\117\000\012\000\117\000\013\000\117\000\
\\014\000\117\000\024\000\117\000\028\000\117\000\034\000\117\000\
\\039\000\117\000\000\000\
\\001\000\001\000\057\000\003\000\056\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\052\000\008\000\051\000\009\000\050\000\
\\010\000\049\000\011\000\048\000\012\000\047\000\013\000\110\000\
\\014\000\110\000\024\000\110\000\028\000\110\000\034\000\110\000\
\\039\000\110\000\000\000\
\\001\000\001\000\057\000\003\000\056\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\052\000\008\000\051\000\009\000\050\000\
\\010\000\049\000\011\000\048\000\012\000\047\000\013\000\111\000\
\\014\000\111\000\024\000\111\000\028\000\111\000\034\000\111\000\
\\039\000\111\000\000\000\
\\001\000\001\000\057\000\003\000\056\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\052\000\008\000\051\000\009\000\050\000\
\\010\000\049\000\011\000\048\000\012\000\047\000\013\000\046\000\
\\014\000\109\000\024\000\109\000\028\000\109\000\034\000\109\000\
\\039\000\109\000\000\000\
\\001\000\001\000\057\000\003\000\056\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\052\000\008\000\051\000\009\000\050\000\
\\010\000\049\000\011\000\048\000\012\000\047\000\013\000\046\000\
\\014\000\045\000\024\000\062\000\000\000\
\\001\000\001\000\057\000\003\000\056\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\052\000\008\000\051\000\009\000\050\000\
\\010\000\049\000\011\000\048\000\012\000\047\000\013\000\046\000\
\\014\000\045\000\028\000\044\000\000\000\
\\001\000\001\000\057\000\003\000\056\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\052\000\008\000\051\000\009\000\050\000\
\\010\000\049\000\011\000\048\000\012\000\047\000\013\000\046\000\
\\014\000\045\000\034\000\095\000\000\000\
\\001\000\001\000\057\000\003\000\056\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\052\000\008\000\051\000\009\000\050\000\
\\010\000\049\000\011\000\048\000\012\000\047\000\013\000\046\000\
\\014\000\045\000\034\000\096\000\000\000\
\\001\000\001\000\057\000\003\000\056\000\004\000\055\000\005\000\054\000\
\\006\000\053\000\007\000\052\000\008\000\051\000\009\000\050\000\
\\010\000\049\000\011\000\048\000\012\000\047\000\013\000\046\000\
\\014\000\045\000\039\000\077\000\000\000\
\\001\000\016\000\026\000\000\000\
\\001\000\017\000\003\000\000\000\
\\001\000\018\000\008\000\036\000\086\000\000\000\
\\001\000\019\000\040\000\020\000\039\000\000\000\
\\001\000\021\000\021\000\022\000\020\000\023\000\019\000\027\000\018\000\
\\037\000\093\000\040\000\017\000\000\000\
\\001\000\025\000\091\000\026\000\091\000\029\000\091\000\042\000\091\000\000\000\
\\001\000\025\000\080\000\000\000\
\\001\000\026\000\082\000\000\000\
\\001\000\029\000\079\000\000\000\
\\001\000\032\000\005\000\000\000\
\\001\000\033\000\089\000\000\000\
\\001\000\033\000\090\000\035\000\023\000\000\000\
\\001\000\033\000\022\000\000\000\
\\001\000\034\000\087\000\000\000\
\\001\000\034\000\088\000\000\000\
\\001\000\034\000\094\000\000\000\
\\001\000\034\000\097\000\000\000\
\\001\000\034\000\098\000\000\000\
\\001\000\034\000\009\000\000\000\
\\001\000\034\000\024\000\000\000\
\\001\000\036\000\085\000\000\000\
\\001\000\036\000\011\000\000\000\
\\001\000\037\000\092\000\000\000\
\\001\000\037\000\025\000\000\000\
\\001\000\040\000\004\000\000\000\
\\001\000\040\000\013\000\000\000\
\\001\000\040\000\038\000\000\000\
\\001\000\041\000\060\000\000\000\
\\001\000\041\000\061\000\000\000\
\\001\000\042\000\000\000\000\000\
\\001\000\042\000\084\000\000\000\
\"
val actionRowNumbers =
"\028\000\051\000\036\000\029\000\
\\045\000\048\000\052\000\029\000\
\\057\000\031\000\039\000\038\000\
\\047\000\046\000\050\000\027\000\
\\012\000\012\000\012\000\053\000\
\\030\000\052\000\031\000\032\000\
\\012\000\023\000\007\000\006\000\
\\012\000\011\000\010\000\012\000\
\\054\000\055\000\022\000\024\000\
\\042\000\041\000\040\000\037\000\
\\049\000\025\000\048\000\012\000\
\\012\000\012\000\012\000\012\000\
\\012\000\012\000\012\000\012\000\
\\012\000\012\000\012\000\012\000\
\\026\000\020\000\008\000\009\000\
\\048\000\035\000\021\000\019\000\
\\016\000\015\000\014\000\013\000\
\\018\000\017\000\004\000\003\000\
\\002\000\001\000\000\000\005\000\
\\033\000\044\000\048\000\034\000\
\\043\000\056\000"
val gotoT =
"\
\\001\000\081\000\000\000\
\\000\000\
\\000\000\
\\002\000\005\000\003\000\004\000\000\000\
\\000\000\
\\007\000\008\000\000\000\
\\004\000\010\000\000\000\
\\002\000\012\000\003\000\004\000\000\000\
\\000\000\
\\005\000\014\000\006\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\025\000\000\000\
\\008\000\034\000\000\000\
\\008\000\035\000\000\000\
\\000\000\
\\000\000\
\\004\000\039\000\000\000\
\\005\000\040\000\006\000\013\000\000\000\
\\000\000\
\\008\000\041\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\056\000\000\000\
\\000\000\
\\000\000\
\\008\000\057\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\061\000\000\000\
\\008\000\062\000\000\000\
\\008\000\063\000\000\000\
\\008\000\064\000\000\000\
\\008\000\065\000\000\000\
\\008\000\066\000\000\000\
\\008\000\067\000\000\000\
\\008\000\068\000\000\000\
\\008\000\069\000\000\000\
\\008\000\070\000\000\000\
\\008\000\071\000\000\000\
\\008\000\072\000\000\000\
\\008\000\073\000\000\000\
\\008\000\074\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\079\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 82
val numrules = 36
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
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | TOK_NUM of unit ->  (int) | TOK_ID of unit ->  (string)
 | expr of unit ->  (EXPR) | cmdseq of unit ->  (CMD list)
 | cmd of unit ->  (CMD) | cmdlist of unit ->  (CMD list)
 | varlist of unit ->  (string list) | dec of unit ->  (DEC list)
 | declist of unit ->  (DEC list) | prog of unit ->  (PROG)
end
type svalue = MlyValue.svalue
type result = PROG
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 16) => true | (T 17) => true | (T 18) => true | (T 19) => true
 | (T 20) => true | (T 21) => true | (T 22) => true | (T 23) => true
 | (T 24) => true | (T 25) => true | (T 26) => true | (T 27) => true
 | (T 28) => true | (T 29) => true | (T 30) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 41) => true | _ => false
val showTerminal =
fn (T 0) => "TOK_ADD"
  | (T 1) => "TOK_UMINUS"
  | (T 2) => "TOK_SUB"
  | (T 3) => "TOK_MUL"
  | (T 4) => "TOK_DIV"
  | (T 5) => "TOK_MOD"
  | (T 6) => "TOK_EQ"
  | (T 7) => "TOK_NE"
  | (T 8) => "TOK_GT"
  | (T 9) => "TOK_GE"
  | (T 10) => "TOK_LT"
  | (T 11) => "TOK_LE"
  | (T 12) => "TOK_AND"
  | (T 13) => "TOK_OR"
  | (T 14) => "TOK_NOT"
  | (T 15) => "TOK_ASSIGN"
  | (T 16) => "TOK_PROGRAM"
  | (T 17) => "TOK_VAR"
  | (T 18) => "TOK_INT"
  | (T 19) => "TOK_BOOL"
  | (T 20) => "TOK_READ"
  | (T 21) => "TOK_WRITE"
  | (T 22) => "TOK_IF"
  | (T 23) => "TOK_THEN"
  | (T 24) => "TOK_ELSE"
  | (T 25) => "TOK_ENDIF"
  | (T 26) => "TOK_WHILE"
  | (T 27) => "TOK_DO"
  | (T 28) => "TOK_ENDWH"
  | (T 29) => "TOK_TT"
  | (T 30) => "TOK_FF"
  | (T 31) => "TOK_BLOCKSTART"
  | (T 32) => "TOK_COLON"
  | (T 33) => "TOK_SEMICOLON"
  | (T 34) => "TOK_COMMA"
  | (T 35) => "TOK_LBRACE"
  | (T 36) => "TOK_RBRACE"
  | (T 37) => "TOK_LPAREN"
  | (T 38) => "TOK_RPAREN"
  | (T 39) => "TOK_ID"
  | (T 40) => "TOK_NUM"
  | (T 41) => "TOK_EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33)
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.cmdseq cmdseq1, _, cmdseq1right)) :: ( _, (
 MlyValue.declist declist1, _, _)) :: _ :: ( _, ( MlyValue.TOK_ID 
TOK_ID1, _, _)) :: ( _, ( _, TOK_PROGRAM1left, _)) :: rest671)) => let
 val  result = MlyValue.prog (fn _ => let val  TOK_ID1 = TOK_ID1 ()
 val  (declist as declist1) = declist1 ()
 val  (cmdseq as cmdseq1) = cmdseq1 ()
 in (PROG (declist, cmdseq))
end)
 in ( LrTable.NT 0, ( result, TOK_PROGRAM1left, cmdseq1right), rest671
)
end
|  ( 1, ( ( _, ( MlyValue.declist declist1, _, declist1right)) :: _ ::
 ( _, ( MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  
result = MlyValue.declist (fn _ => let val  (dec as dec1) = dec1 ()
 val  (declist as declist1) = declist1 ()
 in (prependAll dec declist)
end)
 in ( LrTable.NT 1, ( result, dec1left, declist1right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.declist (fn _ => ([]
))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( _, _, TOK_INT1right)) :: _ :: ( _, ( MlyValue.varlist
 varlist1, _, _)) :: ( _, ( _, TOK_VAR1left, _)) :: rest671)) => let
 val  result = MlyValue.dec (fn _ => let val  (varlist as varlist1) = 
varlist1 ()
 in (makeVarList varlist INT)
end)
 in ( LrTable.NT 2, ( result, TOK_VAR1left, TOK_INT1right), rest671)

end
|  ( 4, ( ( _, ( _, _, TOK_BOOL1right)) :: _ :: ( _, ( 
MlyValue.varlist varlist1, _, _)) :: ( _, ( _, TOK_VAR1left, _)) :: 
rest671)) => let val  result = MlyValue.dec (fn _ => let val  (varlist
 as varlist1) = varlist1 ()
 in (makeVarList varlist BOOL)
end)
 in ( LrTable.NT 2, ( result, TOK_VAR1left, TOK_BOOL1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.varlist varlist1, _, varlist1right)) :: _ ::
 ( _, ( MlyValue.TOK_ID TOK_ID1, TOK_ID1left, _)) :: rest671)) => let
 val  result = MlyValue.varlist (fn _ => let val  (TOK_ID as TOK_ID1)
 = TOK_ID1 ()
 val  (varlist as varlist1) = varlist1 ()
 in (TOK_ID::varlist)
end)
 in ( LrTable.NT 3, ( result, TOK_ID1left, varlist1right), rest671)

end
|  ( 6, ( ( _, ( MlyValue.TOK_ID TOK_ID1, TOK_ID1left, TOK_ID1right))
 :: rest671)) => let val  result = MlyValue.varlist (fn _ => let val 
 (TOK_ID as TOK_ID1) = TOK_ID1 ()
 in ([TOK_ID])
end)
 in ( LrTable.NT 3, ( result, TOK_ID1left, TOK_ID1right), rest671)
end
|  ( 7, ( ( _, ( _, _, TOK_RBRACE1right)) :: ( _, ( MlyValue.cmdlist 
cmdlist1, _, _)) :: ( _, ( _, TOK_LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.cmdseq (fn _ => let val  (cmdlist as cmdlist1)
 = cmdlist1 ()
 in (cmdlist)
end)
 in ( LrTable.NT 6, ( result, TOK_LBRACE1left, TOK_RBRACE1right), 
rest671)
end
|  ( 8, ( ( _, ( MlyValue.cmdlist cmdlist1, _, cmdlist1right)) :: _ ::
 ( _, ( MlyValue.cmd cmd1, cmd1left, _)) :: rest671)) => let val  
result = MlyValue.cmdlist (fn _ => let val  (cmd as cmd1) = cmd1 ()
 val  (cmdlist as cmdlist1) = cmdlist1 ()
 in (cmd::cmdlist)
end)
 in ( LrTable.NT 4, ( result, cmd1left, cmdlist1right), rest671)
end
|  ( 9, ( rest671)) => let val  result = MlyValue.cmdlist (fn _ => ([]
))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 10, ( ( _, ( MlyValue.TOK_ID TOK_ID1, _, TOK_ID1right)) :: ( _, (
 _, TOK_READ1left, _)) :: rest671)) => let val  result = MlyValue.cmd
 (fn _ => let val  (TOK_ID as TOK_ID1) = TOK_ID1 ()
 in (RD TOK_ID)
end)
 in ( LrTable.NT 5, ( result, TOK_READ1left, TOK_ID1right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, 
TOK_WRITE1left, _)) :: rest671)) => let val  result = MlyValue.cmd (fn
 _ => let val  (expr as expr1) = expr1 ()
 in (WR expr)
end)
 in ( LrTable.NT 5, ( result, TOK_WRITE1left, expr1right), rest671)

end
|  ( 12, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.TOK_ID TOK_ID1, TOK_ID1left, _)) :: rest671)) => let val  
result = MlyValue.cmd (fn _ => let val  (TOK_ID as TOK_ID1) = TOK_ID1
 ()
 val  (expr as expr1) = expr1 ()
 in (set TOK_ID expr)
end)
 in ( LrTable.NT 5, ( result, TOK_ID1left, expr1right), rest671)
end
|  ( 13, ( ( _, ( _, _, TOK_ENDIF1right)) :: ( _, ( MlyValue.cmdseq 
cmdseq2, _, _)) :: _ :: ( _, ( MlyValue.cmdseq cmdseq1, _, _)) :: _ ::
 ( _, ( MlyValue.expr expr1, _, _)) :: ( _, ( _, TOK_IF1left, _)) :: 
rest671)) => let val  result = MlyValue.cmd (fn _ => let val  (expr
 as expr1) = expr1 ()
 val  cmdseq1 = cmdseq1 ()
 val  cmdseq2 = cmdseq2 ()
 in (ITE (expr,cmdseq1,cmdseq2))
end)
 in ( LrTable.NT 5, ( result, TOK_IF1left, TOK_ENDIF1right), rest671)

end
|  ( 14, ( ( _, ( _, _, TOK_ENDWH1right)) :: ( _, ( MlyValue.cmdseq 
cmdseq1, _, _)) :: _ :: ( _, ( MlyValue.expr expr1, _, _)) :: ( _, ( _
, TOK_WHILE1left, _)) :: rest671)) => let val  result = MlyValue.cmd
 (fn _ => let val  (expr as expr1) = expr1 ()
 val  (cmdseq as cmdseq1) = cmdseq1 ()
 in (WH (expr,cmdseq))
end)
 in ( LrTable.NT 5, ( result, TOK_WHILE1left, TOK_ENDWH1right), 
rest671)
end
|  ( 15, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (ADD (expr1,expr2))
end)
 in ( LrTable.NT 7, ( result, expr1left, expr2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (SUB (expr1,expr2))
end)
 in ( LrTable.NT 7, ( result, expr1left, expr2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (MUL (expr1,expr2))
end)
 in ( LrTable.NT 7, ( result, expr1left, expr2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (DIV (expr1,expr2))
end)
 in ( LrTable.NT 7, ( result, expr1left, expr2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (MOD (expr1,expr2))
end)
 in ( LrTable.NT 7, ( result, expr1left, expr2right), rest671)
end
|  ( 20, ( ( _, ( _, _, TOK_RPAREN1right)) :: ( _, ( MlyValue.expr 
expr1, _, _)) :: ( _, ( _, TOK_LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.expr (fn _ => let val  (expr as expr1) = expr1
 ()
 in (expr)
end)
 in ( LrTable.NT 7, ( result, TOK_LPAREN1left, TOK_RPAREN1right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.TOK_ID TOK_ID1, TOK_ID1left, TOK_ID1right))
 :: rest671)) => let val  result = MlyValue.expr (fn _ => let val  (
TOK_ID as TOK_ID1) = TOK_ID1 ()
 in (
case lookup (!SymbolTable) TOK_ID of (INT s) => (IREF TOK_ID) | (BOOL b) => (BREF TOK_ID)
)
end)
 in ( LrTable.NT 7, ( result, TOK_ID1left, TOK_ID1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.TOK_NUM TOK_NUM1, TOK_NUM1left, 
TOK_NUM1right)) :: rest671)) => let val  result = MlyValue.expr (fn _
 => let val  (TOK_NUM as TOK_NUM1) = TOK_NUM1 ()
 in (NUM TOK_NUM)
end)
 in ( LrTable.NT 7, ( result, TOK_NUM1left, TOK_NUM1right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.TOK_NUM TOK_NUM1, _, TOK_NUM1right)) :: ( _
, ( _, TOK_UMINUS1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  (TOK_NUM as TOK_NUM1) = TOK_NUM1 ()
 in (NUM (~1*TOK_NUM))
end)
 in ( LrTable.NT 7, ( result, TOK_UMINUS1left, TOK_NUM1right), rest671
)
end
|  ( 24, ( ( _, ( MlyValue.TOK_NUM TOK_NUM1, _, TOK_NUM1right)) :: ( _
, ( _, TOK_ADD1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  (TOK_NUM as TOK_NUM1) = TOK_NUM1 ()
 in (NUM TOK_NUM)
end)
 in ( LrTable.NT 7, ( result, TOK_ADD1left, TOK_NUM1right), rest671)

end
|  ( 25, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (OR (expr1, expr2))
end)
 in ( LrTable.NT 7, ( result, expr1left, expr2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (AND (expr1, expr2))
end)
 in ( LrTable.NT 7, ( result, expr1left, expr2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, 
TOK_NOT1left, _)) :: rest671)) => let val  result = MlyValue.expr (fn
 _ => let val  (expr as expr1) = expr1 ()
 in (NOT expr)
end)
 in ( LrTable.NT 7, ( result, TOK_NOT1left, expr1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (GT (expr1, expr2))
end)
 in ( LrTable.NT 7, ( result, expr1left, expr2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (GE (expr1, expr2))
end)
 in ( LrTable.NT 7, ( result, expr1left, expr2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (LT (expr1, expr2))
end)
 in ( LrTable.NT 7, ( result, expr1left, expr2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (LE (expr1, expr2))
end)
 in ( LrTable.NT 7, ( result, expr1left, expr2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (EQ (expr1, expr2))
end)
 in ( LrTable.NT 7, ( result, expr1left, expr2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (NE (expr1, expr2))
end)
 in ( LrTable.NT 7, ( result, expr1left, expr2right), rest671)
end
|  ( 34, ( ( _, ( _, TOK_TT1left, TOK_TT1right)) :: rest671)) => let
 val  result = MlyValue.expr (fn _ => (TRUE))
 in ( LrTable.NT 7, ( result, TOK_TT1left, TOK_TT1right), rest671)
end
|  ( 35, ( ( _, ( _, TOK_FF1left, TOK_FF1right)) :: rest671)) => let
 val  result = MlyValue.expr (fn _ => (FALSE))
 in ( LrTable.NT 7, ( result, TOK_FF1left, TOK_FF1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : While_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun TOK_ADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_NE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_PROGRAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_BLOCKSTART (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.TOK_ID (fn () => i),p1,p2))
fun TOK_NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.TOK_NUM (fn () => i),p1,p2))
fun TOK_EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
