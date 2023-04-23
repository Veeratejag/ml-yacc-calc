functor RationalLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Rational_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
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


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\168\000\003\000\168\000\004\000\087\000\005\000\086\000\
\\006\000\085\000\011\000\168\000\012\000\168\000\013\000\168\000\
\\014\000\168\000\015\000\168\000\016\000\168\000\017\000\168\000\
\\018\000\168\000\031\000\168\000\035\000\168\000\039\000\168\000\
\\044\000\168\000\000\000\
\\001\000\001\000\169\000\003\000\169\000\004\000\087\000\005\000\086\000\
\\006\000\085\000\011\000\169\000\012\000\169\000\013\000\169\000\
\\014\000\169\000\015\000\169\000\016\000\169\000\017\000\169\000\
\\018\000\169\000\031\000\169\000\035\000\169\000\039\000\169\000\
\\044\000\169\000\000\000\
\\001\000\001\000\170\000\003\000\170\000\004\000\170\000\005\000\170\000\
\\006\000\170\000\011\000\170\000\012\000\170\000\013\000\170\000\
\\014\000\170\000\015\000\170\000\016\000\170\000\017\000\170\000\
\\018\000\170\000\031\000\170\000\035\000\170\000\039\000\170\000\
\\044\000\170\000\000\000\
\\001\000\001\000\171\000\003\000\171\000\004\000\171\000\005\000\171\000\
\\006\000\171\000\011\000\171\000\012\000\171\000\013\000\171\000\
\\014\000\171\000\015\000\171\000\016\000\171\000\017\000\171\000\
\\018\000\171\000\031\000\171\000\035\000\171\000\039\000\171\000\
\\044\000\171\000\000\000\
\\001\000\001\000\172\000\003\000\172\000\004\000\172\000\005\000\172\000\
\\006\000\172\000\011\000\172\000\012\000\172\000\013\000\172\000\
\\014\000\172\000\015\000\172\000\016\000\172\000\017\000\172\000\
\\018\000\172\000\031\000\172\000\035\000\172\000\039\000\172\000\
\\044\000\172\000\000\000\
\\001\000\001\000\173\000\003\000\173\000\004\000\173\000\005\000\173\000\
\\006\000\173\000\011\000\173\000\012\000\173\000\013\000\173\000\
\\014\000\173\000\015\000\173\000\016\000\173\000\017\000\173\000\
\\018\000\173\000\031\000\173\000\035\000\173\000\039\000\173\000\
\\044\000\173\000\000\000\
\\001\000\001\000\174\000\003\000\174\000\004\000\174\000\005\000\174\000\
\\006\000\174\000\011\000\174\000\012\000\174\000\013\000\174\000\
\\014\000\174\000\015\000\174\000\016\000\174\000\017\000\174\000\
\\018\000\174\000\031\000\174\000\035\000\174\000\039\000\174\000\
\\044\000\174\000\000\000\
\\001\000\001\000\089\000\003\000\088\000\004\000\087\000\005\000\086\000\
\\006\000\085\000\011\000\084\000\012\000\083\000\013\000\082\000\
\\014\000\081\000\015\000\080\000\016\000\079\000\000\000\
\\001\000\001\000\089\000\003\000\088\000\004\000\087\000\005\000\086\000\
\\006\000\085\000\011\000\084\000\012\000\083\000\013\000\082\000\
\\014\000\081\000\015\000\080\000\016\000\079\000\039\000\159\000\
\\044\000\159\000\000\000\
\\001\000\001\000\089\000\003\000\088\000\004\000\087\000\005\000\086\000\
\\006\000\085\000\017\000\175\000\018\000\175\000\031\000\175\000\
\\035\000\175\000\039\000\175\000\044\000\175\000\000\000\
\\001\000\001\000\089\000\003\000\088\000\004\000\087\000\005\000\086\000\
\\006\000\085\000\017\000\176\000\018\000\176\000\031\000\176\000\
\\035\000\176\000\039\000\176\000\044\000\176\000\000\000\
\\001\000\001\000\089\000\003\000\088\000\004\000\087\000\005\000\086\000\
\\006\000\085\000\017\000\177\000\018\000\177\000\031\000\177\000\
\\035\000\177\000\039\000\177\000\044\000\177\000\000\000\
\\001\000\001\000\089\000\003\000\088\000\004\000\087\000\005\000\086\000\
\\006\000\085\000\017\000\178\000\018\000\178\000\031\000\178\000\
\\035\000\178\000\039\000\178\000\044\000\178\000\000\000\
\\001\000\001\000\089\000\003\000\088\000\004\000\087\000\005\000\086\000\
\\006\000\085\000\017\000\179\000\018\000\179\000\031\000\179\000\
\\035\000\179\000\039\000\179\000\044\000\179\000\000\000\
\\001\000\001\000\089\000\003\000\088\000\004\000\087\000\005\000\086\000\
\\006\000\085\000\017\000\180\000\018\000\180\000\031\000\180\000\
\\035\000\180\000\039\000\180\000\044\000\180\000\000\000\
\\001\000\002\000\052\000\019\000\051\000\037\000\050\000\038\000\049\000\
\\045\000\065\000\046\000\048\000\048\000\047\000\000\000\
\\001\000\002\000\052\000\019\000\051\000\037\000\050\000\038\000\049\000\
\\046\000\048\000\048\000\047\000\000\000\
\\001\000\002\000\097\000\048\000\047\000\000\000\
\\001\000\002\000\111\000\046\000\048\000\000\000\
\\001\000\007\000\162\000\008\000\162\000\009\000\073\000\010\000\072\000\
\\011\000\162\000\012\000\162\000\013\000\162\000\014\000\162\000\
\\015\000\162\000\016\000\162\000\017\000\162\000\018\000\162\000\
\\031\000\162\000\035\000\162\000\039\000\162\000\044\000\162\000\000\000\
\\001\000\007\000\163\000\008\000\163\000\009\000\073\000\010\000\072\000\
\\011\000\163\000\012\000\163\000\013\000\163\000\014\000\163\000\
\\015\000\163\000\016\000\163\000\017\000\163\000\018\000\163\000\
\\031\000\163\000\035\000\163\000\039\000\163\000\044\000\163\000\000\000\
\\001\000\007\000\164\000\008\000\164\000\009\000\164\000\010\000\164\000\
\\011\000\164\000\012\000\164\000\013\000\164\000\014\000\164\000\
\\015\000\164\000\016\000\164\000\017\000\164\000\018\000\164\000\
\\031\000\164\000\035\000\164\000\039\000\164\000\044\000\164\000\000\000\
\\001\000\007\000\165\000\008\000\165\000\009\000\165\000\010\000\165\000\
\\011\000\165\000\012\000\165\000\013\000\165\000\014\000\165\000\
\\015\000\165\000\016\000\165\000\017\000\165\000\018\000\165\000\
\\031\000\165\000\035\000\165\000\039\000\165\000\044\000\165\000\000\000\
\\001\000\007\000\166\000\008\000\166\000\009\000\166\000\010\000\166\000\
\\011\000\166\000\012\000\166\000\013\000\166\000\014\000\166\000\
\\015\000\166\000\016\000\166\000\017\000\166\000\018\000\166\000\
\\031\000\166\000\035\000\166\000\039\000\166\000\044\000\166\000\000\000\
\\001\000\007\000\167\000\008\000\167\000\009\000\167\000\010\000\167\000\
\\011\000\167\000\012\000\167\000\013\000\167\000\014\000\167\000\
\\015\000\167\000\016\000\167\000\017\000\167\000\018\000\167\000\
\\031\000\167\000\035\000\167\000\039\000\167\000\044\000\167\000\000\000\
\\001\000\007\000\075\000\008\000\074\000\009\000\073\000\010\000\072\000\
\\011\000\071\000\012\000\070\000\013\000\069\000\014\000\068\000\
\\015\000\067\000\016\000\066\000\000\000\
\\001\000\007\000\075\000\008\000\074\000\009\000\073\000\010\000\072\000\
\\011\000\071\000\012\000\070\000\013\000\069\000\014\000\068\000\
\\015\000\067\000\016\000\066\000\039\000\158\000\044\000\158\000\000\000\
\\001\000\007\000\075\000\008\000\074\000\009\000\073\000\010\000\072\000\
\\017\000\181\000\018\000\181\000\031\000\181\000\035\000\181\000\
\\039\000\181\000\044\000\181\000\000\000\
\\001\000\007\000\075\000\008\000\074\000\009\000\073\000\010\000\072\000\
\\017\000\182\000\018\000\182\000\031\000\182\000\035\000\182\000\
\\039\000\182\000\044\000\182\000\000\000\
\\001\000\007\000\075\000\008\000\074\000\009\000\073\000\010\000\072\000\
\\017\000\183\000\018\000\183\000\031\000\183\000\035\000\183\000\
\\039\000\183\000\044\000\183\000\000\000\
\\001\000\007\000\075\000\008\000\074\000\009\000\073\000\010\000\072\000\
\\017\000\184\000\018\000\184\000\031\000\184\000\035\000\184\000\
\\039\000\184\000\044\000\184\000\000\000\
\\001\000\007\000\075\000\008\000\074\000\009\000\073\000\010\000\072\000\
\\017\000\185\000\018\000\185\000\031\000\185\000\035\000\185\000\
\\039\000\185\000\044\000\185\000\000\000\
\\001\000\007\000\075\000\008\000\074\000\009\000\073\000\010\000\072\000\
\\017\000\186\000\018\000\186\000\031\000\186\000\035\000\186\000\
\\039\000\186\000\044\000\186\000\000\000\
\\001\000\017\000\188\000\018\000\188\000\031\000\188\000\035\000\188\000\
\\039\000\188\000\044\000\188\000\000\000\
\\001\000\017\000\189\000\018\000\189\000\031\000\189\000\035\000\189\000\
\\039\000\189\000\044\000\189\000\000\000\
\\001\000\017\000\190\000\018\000\190\000\031\000\190\000\035\000\190\000\
\\039\000\190\000\044\000\190\000\000\000\
\\001\000\017\000\191\000\018\000\191\000\031\000\191\000\035\000\191\000\
\\039\000\191\000\044\000\191\000\000\000\
\\001\000\017\000\078\000\018\000\187\000\031\000\187\000\035\000\187\000\
\\039\000\187\000\044\000\187\000\000\000\
\\001\000\017\000\078\000\018\000\077\000\031\000\093\000\000\000\
\\001\000\017\000\078\000\018\000\077\000\035\000\076\000\000\000\
\\001\000\017\000\078\000\018\000\077\000\039\000\160\000\044\000\160\000\000\000\
\\001\000\020\000\043\000\000\000\
\\001\000\021\000\133\000\041\000\133\000\000\000\
\\001\000\021\000\134\000\023\000\009\000\024\000\008\000\028\000\007\000\
\\041\000\134\000\000\000\
\\001\000\021\000\014\000\041\000\141\000\000\000\
\\001\000\025\000\033\000\026\000\032\000\027\000\031\000\030\000\030\000\
\\034\000\029\000\042\000\145\000\045\000\028\000\000\000\
\\001\000\032\000\143\000\033\000\143\000\036\000\143\000\039\000\143\000\
\\047\000\143\000\000\000\
\\001\000\032\000\126\000\000\000\
\\001\000\033\000\128\000\000\000\
\\001\000\036\000\125\000\000\000\
\\001\000\039\000\131\000\047\000\131\000\000\000\
\\001\000\039\000\135\000\000\000\
\\001\000\039\000\136\000\000\000\
\\001\000\039\000\137\000\000\000\
\\001\000\039\000\138\000\000\000\
\\001\000\039\000\139\000\040\000\038\000\000\000\
\\001\000\039\000\142\000\000\000\
\\001\000\039\000\146\000\000\000\
\\001\000\039\000\147\000\000\000\
\\001\000\039\000\148\000\000\000\
\\001\000\039\000\149\000\000\000\
\\001\000\039\000\150\000\000\000\
\\001\000\039\000\151\000\000\000\
\\001\000\039\000\152\000\000\000\
\\001\000\039\000\153\000\000\000\
\\001\000\039\000\154\000\000\000\
\\001\000\039\000\155\000\000\000\
\\001\000\039\000\156\000\000\000\
\\001\000\039\000\157\000\000\000\
\\001\000\039\000\161\000\044\000\161\000\000\000\
\\001\000\039\000\015\000\000\000\
\\001\000\039\000\034\000\000\000\
\\001\000\039\000\037\000\000\000\
\\001\000\039\000\039\000\000\000\
\\001\000\039\000\040\000\000\000\
\\001\000\039\000\042\000\000\000\
\\001\000\041\000\132\000\000\000\
\\001\000\041\000\140\000\000\000\
\\001\000\041\000\011\000\000\000\
\\001\000\042\000\144\000\000\000\
\\001\000\042\000\041\000\000\000\
\\001\000\043\000\054\000\000\000\
\\001\000\043\000\056\000\000\000\
\\001\000\044\000\123\000\000\000\
\\001\000\044\000\124\000\000\000\
\\001\000\045\000\017\000\000\000\
\\001\000\045\000\035\000\000\000\
\\001\000\045\000\055\000\000\000\
\\001\000\045\000\095\000\000\000\
\\001\000\046\000\092\000\000\000\
\\001\000\046\000\092\000\048\000\091\000\000\000\
\\001\000\047\000\000\000\000\000\
\\001\000\047\000\130\000\000\000\
\\001\000\048\000\091\000\000\000\
\"
val actionRowNumbers =
"\043\000\078\000\092\000\044\000\
\\070\000\085\000\085\000\085\000\
\\050\000\045\000\071\000\076\000\
\\086\000\043\000\072\000\055\000\
\\073\000\074\000\080\000\062\000\
\\061\000\060\000\059\000\058\000\
\\057\000\075\000\041\000\016\000\
\\016\000\081\000\087\000\082\000\
\\044\000\043\000\042\000\051\000\
\\085\000\053\000\052\000\046\000\
\\045\000\015\000\025\000\039\000\
\\007\000\024\000\006\000\036\000\
\\035\000\016\000\090\000\038\000\
\\015\000\064\000\088\000\077\000\
\\056\000\054\000\079\000\026\000\
\\040\000\008\000\063\000\069\000\
\\017\000\017\000\017\000\017\000\
\\017\000\017\000\017\000\017\000\
\\017\000\017\000\078\000\016\000\
\\016\000\018\000\018\000\018\000\
\\018\000\018\000\018\000\018\000\
\\018\000\018\000\018\000\018\000\
\\034\000\023\000\005\000\078\000\
\\083\000\084\000\030\000\093\000\
\\029\000\028\000\027\000\032\000\
\\031\000\022\000\021\000\020\000\
\\019\000\049\000\037\000\033\000\
\\012\000\089\000\011\000\010\000\
\\009\000\014\000\013\000\004\000\
\\003\000\002\000\001\000\000\000\
\\047\000\066\000\065\000\068\000\
\\078\000\048\000\067\000\091\000"
val gotoT =
"\
\\001\000\127\000\002\000\004\000\004\000\003\000\008\000\002\000\
\\019\000\001\000\000\000\
\\005\000\008\000\000\000\
\\000\000\
\\020\000\011\000\024\000\010\000\000\000\
\\000\000\
\\003\000\014\000\000\000\
\\003\000\016\000\000\000\
\\003\000\017\000\000\000\
\\000\000\
\\006\000\025\000\013\000\024\000\014\000\023\000\015\000\022\000\
\\016\000\021\000\017\000\020\000\018\000\019\000\026\000\018\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\004\000\004\000\034\000\000\000\
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
\\009\000\044\000\010\000\043\000\011\000\042\000\000\000\
\\009\000\044\000\010\000\051\000\011\000\042\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\020\000\055\000\024\000\010\000\000\000\
\\002\000\004\000\004\000\003\000\008\000\056\000\019\000\001\000\000\000\
\\000\000\
\\000\000\
\\003\000\057\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\025\000\013\000\024\000\014\000\023\000\015\000\022\000\
\\016\000\021\000\017\000\020\000\018\000\019\000\026\000\058\000\000\000\
\\007\000\062\000\009\000\061\000\010\000\060\000\011\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\044\000\010\000\088\000\011\000\042\000\000\000\
\\000\000\
\\000\000\
\\007\000\092\000\009\000\061\000\010\000\060\000\011\000\059\000\000\000\
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
\\011\000\094\000\000\000\
\\011\000\096\000\000\000\
\\011\000\097\000\000\000\
\\011\000\098\000\000\000\
\\011\000\099\000\000\000\
\\011\000\100\000\000\000\
\\011\000\101\000\000\000\
\\011\000\102\000\000\000\
\\011\000\103\000\000\000\
\\011\000\104\000\000\000\
\\005\000\105\000\000\000\
\\009\000\044\000\010\000\106\000\011\000\042\000\000\000\
\\009\000\044\000\010\000\107\000\011\000\042\000\000\000\
\\009\000\108\000\000\000\
\\009\000\110\000\000\000\
\\009\000\111\000\000\000\
\\009\000\112\000\000\000\
\\009\000\113\000\000\000\
\\009\000\114\000\000\000\
\\009\000\115\000\000\000\
\\009\000\116\000\000\000\
\\009\000\117\000\000\000\
\\009\000\118\000\000\000\
\\009\000\119\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\120\000\000\000\
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
\\005\000\125\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 128
val numrules = 62
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
 | TOK_RAT of unit ->  (RATIONAL) | TOK_NUM of unit ->  (BigInt)
 | TOK_ID of unit ->  (string) | lcmd of unit ->  (CMD list)
 | lident of unit ->  (IDENT list) | procdef of unit ->  (PROCDEF)
 | booldec of unit ->  (BOOLDEC list)
 | intdec of unit ->  (INTDEC list) | ratdec of unit ->  (RATDEC list)
 | procdec of unit ->  (PROCDEC list) | decseq of unit ->  (DECSEQ)
 | wCmd of unit ->  (WCMD) | coCmd of unit ->  (COCMD)
 | pCmd of unit ->  (PCMD) | rCmd of unit ->  (RCMD)
 | cCmd of unit ->  (CCMD) | aCmd of unit ->  (ACMD)
 | ident of unit ->  (IDENT) | rexpr of unit ->  (RATEXPR)
 | bexpr of unit ->  (BOOLEXPR) | iexpr of unit ->  (INTEXPR)
 | block of unit ->  (BLOCK) | expr of unit ->  (EXPR)
 | cmd of unit ->  (CMD) | cmdseq of unit ->  (CMD list)
 | vardec of unit ->  (VARDEC) | varlist of unit ->  (string list)
 | dec of unit ->  (DEC list) | prog of unit ->  (PROG)
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
fn (T 20) => true | (T 21) => true | (T 22) => true | (T 27) => true
 | (T 23) => true | (T 48) => true | (T 49) => true | (T 50) => true
 | (T 51) => true | (T 52) => true | (T 53) => true | (T 54) => true
 | (T 24) => true | (T 25) => true | (T 26) => true | (T 29) => true
 | (T 30) => true | (T 31) => true | (T 32) => true | (T 33) => true
 | (T 34) => true | (T 35) => true | (T 36) => true | (T 37) => true
 | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 46) => true | _ => false
val showTerminal =
fn (T 0) => "TOK_ADD"
  | (T 1) => "TOK_UMINUS"
  | (T 2) => "TOK_SUB"
  | (T 3) => "TOK_MUL"
  | (T 4) => "TOK_DIV"
  | (T 5) => "TOK_MOD"
  | (T 6) => "TOK_ADDR"
  | (T 7) => "TOK_SUBR"
  | (T 8) => "TOK_MULR"
  | (T 9) => "TOK_DIVR"
  | (T 10) => "TOK_EQ"
  | (T 11) => "TOK_NE"
  | (T 12) => "TOK_GT"
  | (T 13) => "TOK_GE"
  | (T 14) => "TOK_LT"
  | (T 15) => "TOK_LE"
  | (T 16) => "TOK_AND"
  | (T 17) => "TOK_OR"
  | (T 18) => "TOK_NOT"
  | (T 19) => "TOK_ASSIGN"
  | (T 20) => "TOK_PROCEDURE"
  | (T 21) => "TOK_VAR"
  | (T 22) => "TOK_INTEGER"
  | (T 23) => "TOK_BOOLEAN"
  | (T 24) => "TOK_READ"
  | (T 25) => "TOK_CALL"
  | (T 26) => "TOK_PRINT"
  | (T 27) => "TOK_RATIONAL"
  | (T 28) => "TOK_WRITE"
  | (T 29) => "TOK_IF"
  | (T 30) => "TOK_THEN"
  | (T 31) => "TOK_ELSE"
  | (T 32) => "TOK_FI"
  | (T 33) => "TOK_WHILE"
  | (T 34) => "TOK_DO"
  | (T 35) => "TOK_OD"
  | (T 36) => "TOK_TT"
  | (T 37) => "TOK_FF"
  | (T 38) => "TOK_SEMICOLON"
  | (T 39) => "TOK_COMMA"
  | (T 40) => "TOK_LBRACE"
  | (T 41) => "TOK_RBRACE"
  | (T 42) => "TOK_LPAREN"
  | (T 43) => "TOK_RPAREN"
  | (T 44) => "TOK_ID"
  | (T 45) => "TOK_NUM"
  | (T 46) => "TOK_EOF"
  | (T 47) => "TOK_RAT"
  | (T 48) => "TOK_MAKERAT"
  | (T 49) => "TOK_FROMINT"
  | (T 50) => "TOK_FROMDECIMAL"
  | (T 51) => "TOK_INVERSE"
  | (T 52) => "TOK_SHOWDECIMAL"
  | (T 53) => "TOK_TODECIMAL"
  | (T 54) => "TOK_SHOWRAT"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 54) $$ (T 53) $$ (T 52) $$ (T 51) $$ (T 50) $$ (T 49) $$ (T 48)
 $$ (T 46) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 
2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.block block1, block1left, block1right)) :: 
rest671)) => let val  result = MlyValue.prog (fn _ => let val  (block
 as block1) = block1 ()
 in (PROG(block))
end)
 in ( LrTable.NT 0, ( result, block1left, block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.cmdseq cmdseq1, _, cmdseq1right)) :: ( _, ( 
MlyValue.decseq decseq1, decseq1left, _)) :: rest671)) => let val  
result = MlyValue.block (fn _ => let val  (decseq as decseq1) = 
decseq1 ()
 val  (cmdseq as cmdseq1) = cmdseq1 ()
 in (BLOCK(decseq,cmdseq))
end)
 in ( LrTable.NT 7, ( result, decseq1left, cmdseq1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.procdec procdec1, _, procdec1right)) :: ( _,
 ( MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) => let val  
result = MlyValue.decseq (fn _ => let val  (vardec as vardec1) = 
vardec1 ()
 val  (procdec as procdec1) = procdec1 ()
 in (DECSEQ(vardec, procdec))
end)
 in ( LrTable.NT 18, ( result, vardec1left, procdec1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.vardec vardec1, _, vardec1right)) :: _ :: (
 _, ( MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  result
 = MlyValue.vardec (fn _ => let val  (dec as dec1) = dec1 ()
 val  (vardec as vardec1) = vardec1 ()
 in (prependAll dec vardec)
end)
 in ( LrTable.NT 3, ( result, dec1left, vardec1right), rest671)
end
|  ( 4, ( rest671)) => let val  result = MlyValue.vardec (fn _ => ([])
)
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 5, ( ( _, ( _, _, TOK_SEMICOLON1right)) :: ( _, ( 
MlyValue.varlist varlist1, _, _)) :: ( _, ( _, TOK_RATIONAL1left, _))
 :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (
varlist as varlist1) = varlist1 ()
 in (makeVarList varlist TOK_RATIONAL)
end)
 in ( LrTable.NT 1, ( result, TOK_RATIONAL1left, TOK_SEMICOLON1right),
 rest671)
end
|  ( 6, ( ( _, ( _, _, TOK_SEMICOLON1right)) :: ( _, ( 
MlyValue.varlist varlist1, _, _)) :: ( _, ( _, TOK_INTEGER1left, _))
 :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (
varlist as varlist1) = varlist1 ()
 in (makeVarList varlist TOK_INTEGER)
end)
 in ( LrTable.NT 1, ( result, TOK_INTEGER1left, TOK_SEMICOLON1right), 
rest671)
end
|  ( 7, ( ( _, ( _, _, TOK_SEMICOLON1right)) :: ( _, ( 
MlyValue.varlist varlist1, _, _)) :: ( _, ( _, TOK_BOOLEAN1left, _))
 :: rest671)) => let val  result = MlyValue.dec (fn _ => let val  (
varlist as varlist1) = varlist1 ()
 in (makeVarList varlist TOK_BOOLEAN)
end)
 in ( LrTable.NT 1, ( result, TOK_BOOLEAN1left, TOK_SEMICOLON1right), 
rest671)
end
|  ( 8, ( ( _, ( MlyValue.varlist varlist1, _, varlist1right)) :: _ ::
 ( _, ( MlyValue.TOK_ID TOK_ID1, TOK_ID1left, _)) :: rest671)) => let
 val  result = MlyValue.varlist (fn _ => let val  (TOK_ID as TOK_ID1)
 = TOK_ID1 ()
 val  (varlist as varlist1) = varlist1 ()
 in (TOK_ID::varlist)
end)
 in ( LrTable.NT 2, ( result, TOK_ID1left, varlist1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.TOK_ID TOK_ID1, TOK_ID1left, TOK_ID1right))
 :: rest671)) => let val  result = MlyValue.varlist (fn _ => let val 
 (TOK_ID as TOK_ID1) = TOK_ID1 ()
 in ([TOK_ID])
end)
 in ( LrTable.NT 2, ( result, TOK_ID1left, TOK_ID1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.procdec procdec1, _, procdec1right)) :: _
 :: ( _, ( MlyValue.procdef procdef1, procdef1left, _)) :: rest671))
 => let val  result = MlyValue.procdec (fn _ => let val  (procdef as 
procdef1) = procdef1 ()
 val  (procdec as procdec1) = procdec1 ()
 in (procdef :: procdec)
end)
 in ( LrTable.NT 19, ( result, procdef1left, procdec1right), rest671)

end
|  ( 11, ( rest671)) => let val  result = MlyValue.procdec (fn _ => (
[]))
 in ( LrTable.NT 19, ( result, defaultPos, defaultPos), rest671)
end
|  ( 12, ( ( _, ( MlyValue.block block1, _, block1right)) :: ( _, ( 
MlyValue.TOK_ID TOK_ID1, _, _)) :: ( _, ( _, TOK_PROCEDURE1left, _))
 :: rest671)) => let val  result = MlyValue.procdef (fn _ => let val 
 (TOK_ID as TOK_ID1) = TOK_ID1 ()
 val  (block as block1) = block1 ()
 in (PROCDEF(TOK_ID,block))
end)
 in ( LrTable.NT 23, ( result, TOK_PROCEDURE1left, block1right), 
rest671)
end
|  ( 13, ( ( _, ( _, _, TOK_RBRACE1right)) :: ( _, ( MlyValue.lcmd 
lcmd1, _, _)) :: ( _, ( _, TOK_LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.cmdseq (fn _ => let val  (lcmd as lcmd1) = 
lcmd1 ()
 in (lcmd)
end)
 in ( LrTable.NT 4, ( result, TOK_LBRACE1left, TOK_RBRACE1right), 
rest671)
end
|  ( 14, ( ( _, ( MlyValue.lcmd lcmd1, _, lcmd1right)) :: _ :: ( _, ( 
MlyValue.cmd cmd1, cmd1left, _)) :: rest671)) => let val  result = 
MlyValue.lcmd (fn _ => let val  (cmd as cmd1) = cmd1 ()
 val  (lcmd as lcmd1) = lcmd1 ()
 in (cmd::lcmd)
end)
 in ( LrTable.NT 25, ( result, cmd1left, lcmd1right), rest671)
end
|  ( 15, ( rest671)) => let val  result = MlyValue.lcmd (fn _ => ([]))
 in ( LrTable.NT 25, ( result, defaultPos, defaultPos), rest671)
end
|  ( 16, ( ( _, ( MlyValue.aCmd aCmd1, aCmd1left, aCmd1right)) :: 
rest671)) => let val  result = MlyValue.cmd (fn _ => let val  (aCmd
 as aCmd1) = aCmd1 ()
 in (ACMD(aCmd))
end)
 in ( LrTable.NT 5, ( result, aCmd1left, aCmd1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.cCmd cCmd1, cCmd1left, cCmd1right)) :: 
rest671)) => let val  result = MlyValue.cmd (fn _ => let val  (cCmd
 as cCmd1) = cCmd1 ()
 in (CCMD(cCmd))
end)
 in ( LrTable.NT 5, ( result, cCmd1left, cCmd1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.rCmd rCmd1, rCmd1left, rCmd1right)) :: 
rest671)) => let val  result = MlyValue.cmd (fn _ => let val  (rCmd
 as rCmd1) = rCmd1 ()
 in (RCMD(rCmd))
end)
 in ( LrTable.NT 5, ( result, rCmd1left, rCmd1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.pCmd pCmd1, pCmd1left, pCmd1right)) :: 
rest671)) => let val  result = MlyValue.cmd (fn _ => let val  (pCmd
 as pCmd1) = pCmd1 ()
 in (PCMD(pCmd))
end)
 in ( LrTable.NT 5, ( result, pCmd1left, pCmd1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.coCmd coCmd1, coCmd1left, coCmd1right)) :: 
rest671)) => let val  result = MlyValue.cmd (fn _ => let val  (coCmd
 as coCmd1) = coCmd1 ()
 in (COCMD(coCmd))
end)
 in ( LrTable.NT 5, ( result, coCmd1left, coCmd1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.wCmd wCmd1, wCmd1left, wCmd1right)) :: 
rest671)) => let val  result = MlyValue.cmd (fn _ => let val  (wCmd
 as wCmd1) = wCmd1 ()
 in (WCMD(wCmd))
end)
 in ( LrTable.NT 5, ( result, wCmd1left, wCmd1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.TOK_ID TOK_ID1, TOK_ID1left, _)) :: rest671)) => let val  
result = MlyValue.aCmd (fn _ => let val  (TOK_ID as TOK_ID1) = TOK_ID1
 ()
 val  (expr as expr1) = expr1 ()
 in (set TOK_ID expr)
end)
 in ( LrTable.NT 12, ( result, TOK_ID1left, expr1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.TOK_ID TOK_ID1, _, TOK_ID1right)) :: ( _, (
 _, TOK_CALL1left, _)) :: rest671)) => let val  result = MlyValue.cCmd
 (fn _ => let val  (TOK_ID as TOK_ID1) = TOK_ID1 ()
 in (call TOK_ID)
end)
 in ( LrTable.NT 13, ( result, TOK_CALL1left, TOK_ID1right), rest671)

end
|  ( 24, ( ( _, ( _, _, TOK_RPAREN1right)) :: ( _, ( MlyValue.TOK_ID 
TOK_ID1, _, _)) :: _ :: ( _, ( _, TOK_READ1left, _)) :: rest671)) =>
 let val  result = MlyValue.rCmd (fn _ => let val  (TOK_ID as TOK_ID1)
 = TOK_ID1 ()
 in (RD TOK_ID)
end)
 in ( LrTable.NT 14, ( result, TOK_READ1left, TOK_RPAREN1right), 
rest671)
end
|  ( 25, ( ( _, ( _, _, TOK_RPAREN1right)) :: ( _, ( MlyValue.expr 
expr1, _, _)) :: _ :: ( _, ( _, TOK_PRINT1left, _)) :: rest671)) =>
 let val  result = MlyValue.pCmd (fn _ => let val  (expr as expr1) = 
expr1 ()
 in (printexpr expr)
end)
 in ( LrTable.NT 15, ( result, TOK_PRINT1left, TOK_RPAREN1right), 
rest671)
end
|  ( 26, ( ( _, ( _, _, TOK_FI1right)) :: ( _, ( MlyValue.cmdseq 
cmdseq2, _, _)) :: _ :: ( _, ( MlyValue.cmdseq cmdseq1, _, _)) :: _ ::
 ( _, ( MlyValue.bexpr bexpr1, _, _)) :: ( _, ( _, TOK_IF1left, _)) ::
 rest671)) => let val  result = MlyValue.coCmd (fn _ => let val  (
bexpr as bexpr1) = bexpr1 ()
 val  cmdseq1 = cmdseq1 ()
 val  cmdseq2 = cmdseq2 ()
 in (ITE (bexpr,cmdseq1,cmdseq2))
end)
 in ( LrTable.NT 16, ( result, TOK_IF1left, TOK_FI1right), rest671)

end
|  ( 27, ( ( _, ( _, _, TOK_OD1right)) :: ( _, ( MlyValue.cmdseq 
cmdseq1, _, _)) :: _ :: ( _, ( MlyValue.bexpr bexpr1, _, _)) :: ( _, (
 _, TOK_WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.wCmd (fn _ => let val  (bexpr as bexpr1) = bexpr1 ()
 val  (cmdseq as cmdseq1) = cmdseq1 ()
 in (WH (bexpr,cmdseq))
end)
 in ( LrTable.NT 17, ( result, TOK_WHILE1left, TOK_OD1right), rest671)

end
|  ( 28, ( ( _, ( MlyValue.rexpr rexpr1, rexpr1left, rexpr1right)) :: 
rest671)) => let val  result = MlyValue.expr (fn _ => let val  (rexpr
 as rexpr1) = rexpr1 ()
 in (REXPR(rexpr))
end)
 in ( LrTable.NT 6, ( result, rexpr1left, rexpr1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.iexpr iexpr1, iexpr1left, iexpr1right)) :: 
rest671)) => let val  result = MlyValue.expr (fn _ => let val  (iexpr
 as iexpr1) = iexpr1 ()
 in (IEXPR(iexpr))
end)
 in ( LrTable.NT 6, ( result, iexpr1left, iexpr1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.bexpr bexpr1, bexpr1left, bexpr1right)) :: 
rest671)) => let val  result = MlyValue.expr (fn _ => let val  (bexpr
 as bexpr1) = bexpr1 ()
 in (BEXPR(bexpr))
end)
 in ( LrTable.NT 6, ( result, bexpr1left, bexpr1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.TOK_ID TOK_ID1, TOK_ID1left, TOK_ID1right))
 :: rest671)) => let val  result = MlyValue.expr (fn _ => let val  (
TOK_ID as TOK_ID1) = TOK_ID1 ()
 in (
case lookup (!SymbolTable) TOK_ID of (INT s) => (IREF TOK_ID) | (BOOL b) => (BREF TOK_ID) | (RAT r)=>(RREF TOK_ID)
)
end)
 in ( LrTable.NT 6, ( result, TOK_ID1left, TOK_ID1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _,
 ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  
result = MlyValue.rexpr (fn _ => let val  rexpr1 = rexpr1 ()
 val  rexpr2 = rexpr2 ()
 in (ADDR(rexpr1,rexpr2))
end)
 in ( LrTable.NT 10, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _,
 ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  
result = MlyValue.rexpr (fn _ => let val  rexpr1 = rexpr1 ()
 val  rexpr2 = rexpr2 ()
 in (SUBR(rexpr1,rexpr2))
end)
 in ( LrTable.NT 10, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _,
 ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  
result = MlyValue.rexpr (fn _ => let val  rexpr1 = rexpr1 ()
 val  rexpr2 = rexpr2 ()
 in (MULR(rexpr1,rexpr2))
end)
 in ( LrTable.NT 10, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _,
 ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  
result = MlyValue.rexpr (fn _ => let val  rexpr1 = rexpr1 ()
 val  rexpr2 = rexpr2 ()
 in (DIVR(rexpr1,rexpr2))
end)
 in ( LrTable.NT 10, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.TOK_RAT TOK_RAT1, _, TOK_RAT1right)) :: ( _
, ( _, TOK_UMINUS1left, _)) :: rest671)) => let val  result = 
MlyValue.rexpr (fn _ => let val  (TOK_RAT as TOK_RAT1) = TOK_RAT1 ()
 in (NEGR(TOK_RAT))
end)
 in ( LrTable.NT 10, ( result, TOK_UMINUS1left, TOK_RAT1right), 
rest671)
end
|  ( 37, ( ( _, ( MlyValue.TOK_RAT TOK_RAT1, TOK_RAT1left, 
TOK_RAT1right)) :: rest671)) => let val  result = MlyValue.rexpr (fn _
 => let val  (TOK_RAT as TOK_RAT1) = TOK_RAT1 ()
 in (TOK_RAT)
end)
 in ( LrTable.NT 10, ( result, TOK_RAT1left, TOK_RAT1right), rest671)

end
|  ( 38, ( ( _, ( MlyValue.iexpr iexpr2, _, iexpr2right)) :: _ :: ( _,
 ( MlyValue.iexpr iexpr1, iexpr1left, _)) :: rest671)) => let val  
result = MlyValue.iexpr (fn _ => let val  iexpr1 = iexpr1 ()
 val  iexpr2 = iexpr2 ()
 in (ADD ( iexpr1, iexpr2))
end)
 in ( LrTable.NT 8, ( result, iexpr1left, iexpr2right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.iexpr iexpr2, _, iexpr2right)) :: _ :: ( _,
 ( MlyValue.iexpr iexpr1, iexpr1left, _)) :: rest671)) => let val  
result = MlyValue.iexpr (fn _ => let val  iexpr1 = iexpr1 ()
 val  iexpr2 = iexpr2 ()
 in (SUB( iexpr1, iexpr2))
end)
 in ( LrTable.NT 8, ( result, iexpr1left, iexpr2right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.iexpr iexpr2, _, iexpr2right)) :: _ :: ( _,
 ( MlyValue.iexpr iexpr1, iexpr1left, _)) :: rest671)) => let val  
result = MlyValue.iexpr (fn _ => let val  iexpr1 = iexpr1 ()
 val  iexpr2 = iexpr2 ()
 in (MUL( iexpr1, iexpr2))
end)
 in ( LrTable.NT 8, ( result, iexpr1left, iexpr2right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.iexpr iexpr2, _, iexpr2right)) :: _ :: ( _,
 ( MlyValue.iexpr iexpr1, iexpr1left, _)) :: rest671)) => let val  
result = MlyValue.iexpr (fn _ => let val  iexpr1 = iexpr1 ()
 val  iexpr2 = iexpr2 ()
 in (DIV( iexpr1, iexpr2))
end)
 in ( LrTable.NT 8, ( result, iexpr1left, iexpr2right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.iexpr iexpr2, _, iexpr2right)) :: _ :: ( _,
 ( MlyValue.iexpr iexpr1, iexpr1left, _)) :: rest671)) => let val  
result = MlyValue.iexpr (fn _ => let val  iexpr1 = iexpr1 ()
 val  iexpr2 = iexpr2 ()
 in (MOD( iexpr1, iexpr2))
end)
 in ( LrTable.NT 8, ( result, iexpr1left, iexpr2right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.TOK_NUM TOK_NUM1, _, TOK_NUM1right)) :: ( _
, ( _, TOK_UMINUS1left, _)) :: rest671)) => let val  result = 
MlyValue.iexpr (fn _ => let val  (TOK_NUM as TOK_NUM1) = TOK_NUM1 ()
 in (NEG(TOK_NUM))
end)
 in ( LrTable.NT 8, ( result, TOK_UMINUS1left, TOK_NUM1right), rest671
)
end
|  ( 44, ( ( _, ( MlyValue.TOK_NUM TOK_NUM1, TOK_NUM1left, 
TOK_NUM1right)) :: rest671)) => let val  result = MlyValue.iexpr (fn _
 => let val  (TOK_NUM as TOK_NUM1) = TOK_NUM1 ()
 in (TOK_NUM)
end)
 in ( LrTable.NT 8, ( result, TOK_NUM1left, TOK_NUM1right), rest671)

end
|  ( 45, ( ( _, ( MlyValue.iexpr iexpr2, _, iexpr2right)) :: _ :: ( _,
 ( MlyValue.iexpr iexpr1, iexpr1left, _)) :: rest671)) => let val  
result = MlyValue.bexpr (fn _ => let val  iexpr1 = iexpr1 ()
 val  iexpr2 = iexpr2 ()
 in (GT (iexpr1,iexpr2))
end)
 in ( LrTable.NT 9, ( result, iexpr1left, iexpr2right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.iexpr iexpr2, _, iexpr2right)) :: _ :: ( _,
 ( MlyValue.iexpr iexpr1, iexpr1left, _)) :: rest671)) => let val  
result = MlyValue.bexpr (fn _ => let val  iexpr1 = iexpr1 ()
 val  iexpr2 = iexpr2 ()
 in (GE (iexpr1,iexpr2))
end)
 in ( LrTable.NT 9, ( result, iexpr1left, iexpr2right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.iexpr iexpr2, _, iexpr2right)) :: _ :: ( _,
 ( MlyValue.iexpr iexpr1, iexpr1left, _)) :: rest671)) => let val  
result = MlyValue.bexpr (fn _ => let val  iexpr1 = iexpr1 ()
 val  iexpr2 = iexpr2 ()
 in (LT (iexpr1,iexpr2))
end)
 in ( LrTable.NT 9, ( result, iexpr1left, iexpr2right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.iexpr iexpr2, _, iexpr2right)) :: _ :: ( _,
 ( MlyValue.iexpr iexpr1, iexpr1left, _)) :: rest671)) => let val  
result = MlyValue.bexpr (fn _ => let val  iexpr1 = iexpr1 ()
 val  iexpr2 = iexpr2 ()
 in (LE (iexpr1,iexpr2))
end)
 in ( LrTable.NT 9, ( result, iexpr1left, iexpr2right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.iexpr iexpr2, _, iexpr2right)) :: _ :: ( _,
 ( MlyValue.iexpr iexpr1, iexpr1left, _)) :: rest671)) => let val  
result = MlyValue.bexpr (fn _ => let val  iexpr1 = iexpr1 ()
 val  iexpr2 = iexpr2 ()
 in (EQ(iexpr1,iexpr2))
end)
 in ( LrTable.NT 9, ( result, iexpr1left, iexpr2right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.iexpr iexpr2, _, iexpr2right)) :: _ :: ( _,
 ( MlyValue.iexpr iexpr1, iexpr1left, _)) :: rest671)) => let val  
result = MlyValue.bexpr (fn _ => let val  iexpr1 = iexpr1 ()
 val  iexpr2 = iexpr2 ()
 in (NE(iexpr1,iexpr2))
end)
 in ( LrTable.NT 9, ( result, iexpr1left, iexpr2right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _,
 ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  
result = MlyValue.bexpr (fn _ => let val  rexpr1 = rexpr1 ()
 val  rexpr2 = rexpr2 ()
 in (GTR(rexpr1,rexpr2))
end)
 in ( LrTable.NT 9, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _,
 ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  
result = MlyValue.bexpr (fn _ => let val  rexpr1 = rexpr1 ()
 val  rexpr2 = rexpr2 ()
 in (GER(rexpr1,rexpr2))
end)
 in ( LrTable.NT 9, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _,
 ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  
result = MlyValue.bexpr (fn _ => let val  rexpr1 = rexpr1 ()
 val  rexpr2 = rexpr2 ()
 in (LTR(rexpr1,rexpr2))
end)
 in ( LrTable.NT 9, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _,
 ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  
result = MlyValue.bexpr (fn _ => let val  rexpr1 = rexpr1 ()
 val  rexpr2 = rexpr2 ()
 in (LER(rexpr1,rexpr2))
end)
 in ( LrTable.NT 9, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _,
 ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  
result = MlyValue.bexpr (fn _ => let val  rexpr1 = rexpr1 ()
 val  rexpr2 = rexpr2 ()
 in (EQR(rexpr1,rexpr2))
end)
 in ( LrTable.NT 9, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _,
 ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  
result = MlyValue.bexpr (fn _ => let val  rexpr1 = rexpr1 ()
 val  rexpr2 = rexpr2 ()
 in (NER(rexpr1,rexpr2))
end)
 in ( LrTable.NT 9, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.bexpr bexpr2, _, bexpr2right)) :: _ :: ( _,
 ( MlyValue.bexpr bexpr1, bexpr1left, _)) :: rest671)) => let val  
result = MlyValue.bexpr (fn _ => let val  bexpr1 = bexpr1 ()
 val  bexpr2 = bexpr2 ()
 in (OR(bexpr1,bexpr2))
end)
 in ( LrTable.NT 9, ( result, bexpr1left, bexpr2right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.bexpr bexpr2, _, bexpr2right)) :: _ :: ( _,
 ( MlyValue.bexpr bexpr1, bexpr1left, _)) :: rest671)) => let val  
result = MlyValue.bexpr (fn _ => let val  bexpr1 = bexpr1 ()
 val  bexpr2 = bexpr2 ()
 in (AND(bexpr1, bexpr2))
end)
 in ( LrTable.NT 9, ( result, bexpr1left, bexpr2right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.bexpr bexpr1, _, bexpr1right)) :: ( _, ( _,
 TOK_NOT1left, _)) :: rest671)) => let val  result = MlyValue.bexpr
 (fn _ => let val  (bexpr as bexpr1) = bexpr1 ()
 in (NOT(bexpr))
end)
 in ( LrTable.NT 9, ( result, TOK_NOT1left, bexpr1right), rest671)
end
|  ( 60, ( ( _, ( _, TOK_TT1left, TOK_TT1right)) :: rest671)) => let
 val  result = MlyValue.bexpr (fn _ => (TRUE))
 in ( LrTable.NT 9, ( result, TOK_TT1left, TOK_TT1right), rest671)
end
|  ( 61, ( ( _, ( _, TOK_FF1left, TOK_FF1right)) :: rest671)) => let
 val  result = MlyValue.bexpr (fn _ => (FALSE))
 in ( LrTable.NT 9, ( result, TOK_FF1left, TOK_FF1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Rational_TOKENS =
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
fun TOK_ADDR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_SUBR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_MULR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_DIVR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_NE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_PROCEDURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_INTEGER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_BOOLEAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_CALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_RATIONAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_OD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.TOK_ID (fn () => i),p1,p2))
fun TOK_NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.TOK_NUM (fn () => i),p1,p2))
fun TOK_EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_RAT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.TOK_RAT (fn () => i),p1,p2))
fun TOK_MAKERAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_FROMINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_FROMDECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_INVERSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_SHOWDECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 52,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_TODECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 53,(
ParserData.MlyValue.VOID,p1,p2))
fun TOK_SHOWRAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 54,(
ParserData.MlyValue.VOID,p1,p2))
end
end
