(* glue.sml Create a lexer and a parser *)

structure Rational_PL0LrVals = Rational_PL0LrValsFun(
structure Token = LrParser.Token);
structure Rational_PL0Lex = Rational_PL0LexFun(
structure Tokens = Rational_PL0LrVals.Tokens);
structure Rational_PL0Parser = JoinWithArg(
structure ParserData = Rational_PL0LrVals.ParserData
structure Lex=Rational_PL0Lex
structure LrParser=LrParser);