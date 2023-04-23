functor RatLexFun(structure Tokens: Rat_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
INITIAL
    structure UserDeclarations = 
      struct

structure T = Tokens

type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token
type lexarg = string
type arg = lexarg

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;

val eof = fn filename => (lin := 1; col := 0; T.TOK_EOF (!lin, !col));

fun inc a = a := !a + 1



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
#[([(#"\^@",#"\b",1),
(#"\v",#"\f",1),
(#"\^N",#"\^_",1),
(#"\"",#"$",1),
(#"'",#"'",1),
(#"?",#"@",1),
(#"[",#"`",1),
(#"\127",#"\255",1),
(#"\t",#"\t",2),
(#" ",#" ",2),
(#"\n",#"\n",3),
(#"\r",#"\r",4),
(#"!",#"!",5),
(#"%",#"%",6),
(#"&",#"&",7),
(#"(",#"(",8),
(#")",#")",9),
(#"*",#"*",10),
(#"+",#"+",11),
(#",",#",",12),
(#"-",#"-",13),
(#".",#".",14),
(#"/",#"/",15),
(#"0",#"9",16),
(#":",#":",17),
(#";",#";",18),
(#"<",#"<",19),
(#"=",#"=",20),
(#">",#">",21),
(#"A",#"Z",22),
(#"a",#"a",22),
(#"g",#"h",22),
(#"j",#"l",22),
(#"n",#"n",22),
(#"q",#"q",22),
(#"u",#"u",22),
(#"x",#"z",22),
(#"b",#"b",23),
(#"c",#"c",24),
(#"d",#"d",25),
(#"e",#"e",26),
(#"f",#"f",27),
(#"i",#"i",28),
(#"m",#"m",29),
(#"o",#"o",30),
(#"p",#"p",31),
(#"r",#"r",32),
(#"s",#"s",33),
(#"t",#"t",34),
(#"v",#"v",35),
(#"w",#"w",36),
(#"{",#"{",37),
(#"|",#"|",38),
(#"}",#"}",39),
(#"~",#"~",40)], [0]), ([], [56]), ([(#"\t",#"\t",157),
(#" ",#" ",157)], [0, 56]), ([], [1]), ([(#"\n",#"\n",3)], [1, 56]), ([], [50, 56]), ([], [41, 56]), ([(#"&",#"&",156)], [56]), ([(#"*",#"*",153)], [28, 56]), ([], [29, 56]), ([], [39, 56]), ([], [36, 56]), ([], [30, 56]), ([], [38, 56]), ([(#"*",#"*",145),
(#"+",#"+",146),
(#"-",#"-",147),
(#"/",#"/",148)], [56]), ([], [40, 56]), ([(#"0",#"9",144)], [54, 56]), ([(#"=",#"=",143)], [56]), ([], [31, 56]), ([(#"=",#"=",141),
(#">",#">",142)], [46, 56]), ([], [42, 56]), ([(#"=",#"=",140)], [44, 56]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [55, 56]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"n",42),
(#"p",#"z",42),
(#"o",#"o",134)], [55, 56]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"b",#"z",42),
(#"a",#"a",131)], [55, 56]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"n",42),
(#"p",#"z",42),
(#"o",#"o",130)], [55, 56]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"k",42),
(#"m",#"z",42),
(#"l",#"l",127)], [55, 56]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"e",42),
(#"g",#"h",42),
(#"j",#"q",42),
(#"s",#"z",42),
(#"f",#"f",115),
(#"i",#"i",116),
(#"r",#"r",117)], [55, 56]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"e",42),
(#"g",#"m",42),
(#"o",#"z",42),
(#"f",#"f",103),
(#"n",#"n",104)], [55, 56]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"b",#"z",42),
(#"a",#"a",96)], [55, 56]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"c",42),
(#"e",#"z",42),
(#"d",#"d",95)], [55, 56]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"q",42),
(#"s",#"z",42),
(#"r",#"r",84)], [55, 56]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"b",#"d",42),
(#"f",#"z",42),
(#"a",#"a",74),
(#"e",#"e",75)], [55, 56]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"g",42),
(#"i",#"z",42),
(#"h",#"h",61)], [55, 56]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"g",42),
(#"i",#"n",42),
(#"p",#"s",42),
(#"u",#"z",42),
(#"h",#"h",49),
(#"o",#"o",50),
(#"t",#"t",51)], [55, 56]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"b",#"z",42),
(#"a",#"a",47)], [55, 56]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"g",42),
(#"i",#"z",42),
(#"h",#"h",43)], [55, 56]), ([], [26, 56]), ([(#"|",#"|",41)], [56]), ([], [27, 56]), ([], [37, 56]), ([], [49]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"h",42),
(#"j",#"z",42),
(#"i",#"i",44)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"k",42),
(#"m",#"z",42),
(#"l",#"l",45)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"d",42),
(#"f",#"z",42),
(#"e",#"e",46)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [2, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"q",42),
(#"s",#"z",42),
(#"r",#"r",48)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [13, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"d",42),
(#"f",#"z",42),
(#"e",#"e",59)], [55]), ([(#"0",#"9",42),
(#"A",#"C",42),
(#"E",#"Z",42),
(#"a",#"z",42),
(#"D",#"D",52)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [17, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"d",42),
(#"f",#"z",42),
(#"e",#"e",53)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"b",42),
(#"d",#"z",42),
(#"c",#"c",54)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"h",42),
(#"j",#"z",42),
(#"i",#"i",55)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"l",42),
(#"n",#"z",42),
(#"m",#"m",56)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"b",#"z",42),
(#"a",#"a",57)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"k",42),
(#"m",#"z",42),
(#"l",#"l",58)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [25, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"m",42),
(#"o",#"z",42),
(#"n",#"n",60)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [6, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"n",42),
(#"p",#"z",42),
(#"o",#"o",62)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"v",42),
(#"x",#"z",42),
(#"w",#"w",63)], [55]), ([(#"0",#"9",42),
(#"A",#"C",42),
(#"E",#"Q",42),
(#"S",#"Z",42),
(#"a",#"z",42),
(#"D",#"D",64),
(#"R",#"R",65)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"d",42),
(#"f",#"z",42),
(#"e",#"e",68)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"b",#"z",42),
(#"a",#"a",66)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"s",42),
(#"u",#"z",42),
(#"t",#"t",67)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [22, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"b",42),
(#"d",#"z",42),
(#"c",#"c",69)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"h",42),
(#"j",#"z",42),
(#"i",#"i",70)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"l",42),
(#"n",#"z",42),
(#"m",#"m",71)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"b",#"z",42),
(#"a",#"a",72)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"k",42),
(#"m",#"z",42),
(#"l",#"l",73)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [23, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"s",42),
(#"u",#"z",42),
(#"t",#"t",78)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"b",#"z",42),
(#"a",#"a",76)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"c",42),
(#"e",#"z",42),
(#"d",#"d",77)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [11, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"h",42),
(#"j",#"z",42),
(#"i",#"i",79)], [21, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"n",42),
(#"p",#"z",42),
(#"o",#"o",80)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"m",42),
(#"o",#"z",42),
(#"n",#"n",81)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"b",#"z",42),
(#"a",#"a",82)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"k",42),
(#"m",#"z",42),
(#"l",#"l",83)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [16, 53, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"h",42),
(#"j",#"n",42),
(#"p",#"z",42),
(#"i",#"i",85),
(#"o",#"o",86)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"m",42),
(#"o",#"z",42),
(#"n",#"n",93)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"b",42),
(#"d",#"z",42),
(#"c",#"c",87)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"d",42),
(#"f",#"z",42),
(#"e",#"e",88)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"c",42),
(#"e",#"z",42),
(#"d",#"d",89)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"t",42),
(#"v",#"z",42),
(#"u",#"u",90)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"q",42),
(#"s",#"z",42),
(#"r",#"r",91)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"d",42),
(#"f",#"z",42),
(#"e",#"e",92)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [12, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"s",42),
(#"u",#"z",42),
(#"t",#"t",94)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [9, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [4, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"j",42),
(#"l",#"z",42),
(#"k",#"k",97)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"d",42),
(#"f",#"z",42),
(#"e",#"e",98)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42),
(#"_",#"_",99)], [55]), ([(#"r",#"r",100)], []), ([(#"a",#"a",101)], []), ([(#"t",#"t",102)], []), ([], [20]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [5, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"s",42),
(#"u",#"u",42),
(#"w",#"z",42),
(#"t",#"t",105),
(#"v",#"v",106)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"d",42),
(#"f",#"z",42),
(#"e",#"e",111)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"d",42),
(#"f",#"z",42),
(#"e",#"e",107)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"q",42),
(#"s",#"z",42),
(#"r",#"r",108)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"r",42),
(#"t",#"z",42),
(#"s",#"s",109)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"d",42),
(#"f",#"z",42),
(#"e",#"e",110)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [19, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"f",42),
(#"h",#"z",42),
(#"g",#"g",112)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"d",42),
(#"f",#"z",42),
(#"e",#"e",113)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"q",42),
(#"s",#"z",42),
(#"r",#"r",114)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [14, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [18, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [8, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"n",42),
(#"p",#"z",42),
(#"o",#"o",118)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"l",42),
(#"n",#"z",42),
(#"m",#"m",119)], [55]), ([(#"0",#"9",42),
(#"A",#"C",42),
(#"E",#"Z",42),
(#"a",#"z",42),
(#"D",#"D",120)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"d",42),
(#"f",#"z",42),
(#"e",#"e",121)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"b",42),
(#"d",#"z",42),
(#"c",#"c",122)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"h",42),
(#"j",#"z",42),
(#"i",#"i",123)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"l",42),
(#"n",#"z",42),
(#"m",#"m",124)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"b",#"z",42),
(#"a",#"a",125)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"k",42),
(#"m",#"z",42),
(#"l",#"l",126)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [24, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"r",42),
(#"t",#"z",42),
(#"s",#"s",128)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"d",42),
(#"f",#"z",42),
(#"e",#"e",129)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [7, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [3, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"k",42),
(#"m",#"z",42),
(#"l",#"l",132)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"k",42),
(#"m",#"z",42),
(#"l",#"l",133)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [10, 55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"n",42),
(#"p",#"z",42),
(#"o",#"o",135)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"k",42),
(#"m",#"z",42),
(#"l",#"l",136)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"d",42),
(#"f",#"z",42),
(#"e",#"e",137)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"b",#"z",42),
(#"a",#"a",138)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"m",42),
(#"o",#"z",42),
(#"n",#"n",139)], [55]), ([(#"0",#"9",42),
(#"A",#"Z",42),
(#"a",#"z",42)], [15, 55]), ([], [45]), ([], [47]), ([], [43]), ([], [51]), ([(#"0",#"9",144)], [54]), ([(#".",#".",152)], []), ([(#".",#".",151)], []), ([(#".",#".",150)], []), ([(#".",#".",149)], []), ([], [35]), ([], [33]), ([], [32]), ([], [34]), ([(#"\^@",#")",153),
(#"+",#"\255",153),
(#"*",#"*",154)], []), ([(#"\^@",#"(",153),
(#"+",#"\255",153),
(#")",#")",155),
(#"*",#"*",154)], []), ([(#"\^@",#")",153),
(#"+",#"\255",153),
(#"*",#"*",154)], [52]), ([], [48]), ([(#"\t",#"\t",157),
(#" ",#" ",157)], [0])]
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as (fileName: string)) () = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm; (continue ()))
fun yyAction1 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (inc lin; eolpos:=yypos+size yytext; continue ())
      end
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_WHILE(!lin,!col)))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_DO(!lin,!col)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_ENDWH(!lin,!col)))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_IF(!lin,!col)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_THEN(!lin,!col)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_ELSE(!lin,!col)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_ENDIF(!lin,!col)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_PRINT(!lin,!col)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_CALL(!lin,!col)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_READ(!lin,!col)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_PROCEDURE(!lin,!col)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_VAR(!lin,!col)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_INTEGER(!lin,!col)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_BOOLEAN(!lin,!col)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_RATIONAL(!lin,!col)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_TT(!lin,!col)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_FF(!lin,!col)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_INVERSE(!lin,!col)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_MAKERAT(!lin,!col)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_FROMINT(!lin,!col)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_SHOWRAT(!lin,!col)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_SHOWDECIMAL(!lin,!col)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_FROMDECIMAL(!lin,!col)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_TODECIMAL(!lin,!col)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_LBRACE(!lin,!col)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_RBRACE(!lin,!col)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_LPAREN(!lin,!col)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_RPAREN(!lin,!col)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_COMMA(!lin,!col)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_SEMICOLON(!lin,!col)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_ADDR(!lin,!col)))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_SUBR(!lin,!col)))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_MULR(!lin,!col)))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_DIVR(!lin,!col)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_ADD(!lin,!col)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_UMINUS(!lin,!col)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_SUB(!lin,!col)))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_MUL(!lin,!col)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_DIV(!lin,!col)))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_MOD(!lin,!col)))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_EQ(!lin,!col)))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_NE(!lin,!col)))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_GT(!lin,!col)))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_GE(!lin,!col)))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_LT(!lin,!col)))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_LE(!lin,!col)))
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_AND(!lin,!col)))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_OR(!lin,!col)))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_NOT(!lin,!col)))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos); T.TOK_ASSIGN(!lin,!col)))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm; (lex()))
fun yyAction53 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col:=yypos-(!eolpos); 
        T.TOK_RAT( Rational.fromDecimal (String.substring(yytext,!lin,!col))))
      end
fun yyAction54 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col:=yypos-(!eolpos);
        T.TOK_NUM(BigInt.fromStr(String.substring(yytext, !lin, !col))))
      end
fun yyAction55 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col:=yypos-(!eolpos);
        T.TOK_ID(yytext,!lin,!col))
      end
fun yyAction56 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (print ("Unknown token found at " ^ (Int.toString (!lin)) ^ ": <" ^ yytext ^ ">. Continuing.\n"); continue())
      end
val yyactTable = Vector.fromList([yyAction0, yyAction1, yyAction2, yyAction3,
  yyAction4, yyAction5, yyAction6, yyAction7, yyAction8, yyAction9, yyAction10,
  yyAction11, yyAction12, yyAction13, yyAction14, yyAction15, yyAction16,
  yyAction17, yyAction18, yyAction19, yyAction20, yyAction21, yyAction22,
  yyAction23, yyAction24, yyAction25, yyAction26, yyAction27, yyAction28,
  yyAction29, yyAction30, yyAction31, yyAction32, yyAction33, yyAction34,
  yyAction35, yyAction36, yyAction37, yyAction38, yyAction39, yyAction40,
  yyAction41, yyAction42, yyAction43, yyAction44, yyAction45, yyAction46,
  yyAction47, yyAction48, yyAction49, yyAction50, yyAction51, yyAction52,
  yyAction53, yyAction54, yyAction55, yyAction56])
in
  if yyInput.eof(!(yystrm))
    then UserDeclarations.eof(yyarg)
    else (case (!(yyss))
       of INITIAL => yygo yyactTable (0, !(yystrm), yyNO_MATCH)
      (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
