structure RatLrVals = RatLrValsFun(structure Token = LrParser.Token);
structure RatLex = RatLexFun(structure Tokens = RatLrVals.Tokens);
structure RatParser = JoinWithArg(
            structure ParserData = RatLrVals.ParserData
            structure Lex=RatLex
            structure LrParser=LrParser);

