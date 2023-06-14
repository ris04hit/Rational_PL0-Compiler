signature Rational_PL0_TOKENS =
sig
type ('a,'b) token
type svalue
val BAD_CHARACTER:  'a * 'a -> (svalue,'a) token
val BOOLEAN_RES:  'a * 'a -> (svalue,'a) token
val INTEGER_RES:  'a * 'a -> (svalue,'a) token
val RATIONAL_RES:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
val COMMENT:  'a * 'a -> (svalue,'a) token
val IDENTIFIER: (string) *  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val EOS:  'a * 'a -> (svalue,'a) token
val RCURLYBRACE:  'a * 'a -> (svalue,'a) token
val LCURLYBRACE:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val ASSIGNMENT:  'a * 'a -> (svalue,'a) token
val GREATER_EQUAL:  'a * 'a -> (svalue,'a) token
val GREATER:  'a * 'a -> (svalue,'a) token
val LESS_EQUAL:  'a * 'a -> (svalue,'a) token
val LESS:  'a * 'a -> (svalue,'a) token
val NOT_EQUAL:  'a * 'a -> (svalue,'a) token
val EQUAL:  'a * 'a -> (svalue,'a) token
val OR_BOOL:  'a * 'a -> (svalue,'a) token
val AND_BOOL:  'a * 'a -> (svalue,'a) token
val NEGATE_BOOL:  'a * 'a -> (svalue,'a) token
val MOD_INT:  'a * 'a -> (svalue,'a) token
val DIVIDE_INT:  'a * 'a -> (svalue,'a) token
val MULTIPLY_INT:  'a * 'a -> (svalue,'a) token
val DIVIDE_RAT:  'a * 'a -> (svalue,'a) token
val MULTIPLY_RAT:  'a * 'a -> (svalue,'a) token
val SUBTRACT_RAT:  'a * 'a -> (svalue,'a) token
val ADD_RAT:  'a * 'a -> (svalue,'a) token
val SUBTRACT_INT:  'a * 'a -> (svalue,'a) token
val PLUS_SIGN:  'a * 'a -> (svalue,'a) token
val MINUS_SIGN:  'a * 'a -> (svalue,'a) token
val TODECIMAL:  'a * 'a -> (svalue,'a) token
val FROMDECIMAL:  'a * 'a -> (svalue,'a) token
val SHOWDECIMAL:  'a * 'a -> (svalue,'a) token
val SHOWRAT:  'a * 'a -> (svalue,'a) token
val RAT:  'a * 'a -> (svalue,'a) token
val MAKE_RAT:  'a * 'a -> (svalue,'a) token
val INVERSE:  'a * 'a -> (svalue,'a) token
val CALL:  'a * 'a -> (svalue,'a) token
val READ:  'a * 'a -> (svalue,'a) token
val PRINT:  'a * 'a -> (svalue,'a) token
val PROCEDURE:  'a * 'a -> (svalue,'a) token
val OD:  'a * 'a -> (svalue,'a) token
val DO:  'a * 'a -> (svalue,'a) token
val WHILE:  'a * 'a -> (svalue,'a) token
val FI:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
val BOOLEAN: (bool) *  'a * 'a -> (svalue,'a) token
val INTEG: (BigInt.bigint) *  'a * 'a -> (svalue,'a) token
val RATION: (Rational.rational) *  'a * 'a -> (svalue,'a) token
end
signature Rational_PL0_LRVALS=
sig
structure Tokens : Rational_PL0_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
