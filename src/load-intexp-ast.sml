CM.make("$/basis.cm"); (* Loads SML basis library *)
CM.make("$/ml-yacc-lib.cm"); (* loads SML YACC library *)

use "AST.sml"; (* datatype for integer expression abstract syntax trees *)

use "Intexp.yacc.sig"; (* defines Intexp_TOKENS and other datatypes *)

use "Intexp.yacc.sml"; (* defines shift-reduce parser *)

use "Intexp.lex.sml"; (* load lexer *after* parser, since it 
                         uses tokens defined by the parser *)

use "Intexp.sml"; (* top-level parser interface *)

(* These are print parameters, set so that we
   may see all the details of parse trees and strings
 *)

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open Intexp; (* open the parsing module so that we can use parseString
                and parseFile without qualification. *)
