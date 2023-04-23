(* compiler.sml *)
signature RAT =
sig
    exception RatError;
    val compile : string -> DataTypes.PROG
end

structure rat : RAT =
struct
    exception RatError;

    fun compile (fileName) =
    let 
        val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn 
            n => if TextIO.endOfStream inStream
                 then ""
                 else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
            (msg,line,col) =>
                print (fileName^"["^Int.toString line^":"
                      ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = RatParser.parse(15,(RatParser.makeLexer grab fileName),printError,fileName)
            handle RatParser.ParseError => raise RatError;
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;
    in 
        tree
    end

end;