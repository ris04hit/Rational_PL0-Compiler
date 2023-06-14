(* pi.lex *)
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
val badCh : string * string * int * int -> unit = fn(fileName,bad,line,col) => TextIO.output(TextIO.stdOut,fileName^"["^Int.toString line^"."^Int.toString col^"] Invalid character \""^bad^"\"\n");
val eof = fn fileName => T.EOF (!lin,!col);

 (* Keyword management *)
structure KeyWord :
    sig
    val find : string -> (int * int -> (svalue,int) token) option
    end =
struct
    val TableSize = 422 (* 211 *)
    val HashFactor = 5
    val hash = fn
        s => List.foldr (fn (c,v) =>
        (v*HashFactor+(ord c)) mod TableSize) 0 (explode s)
    val HashTable = Array.array(TableSize,nil) :
        (string * (int * int -> (svalue,int) token))
        list Array.array
    val add = fn
        (s,v) => let val i = hash s
            in Array.update(HashTable,i,(s,v)
            :: (Array.sub(HashTable, i)))
            end
    val find = fn
        s => let val i = hash s
            fun f ((key,v)::r) = if s=key then SOME v
                    else f r
                | f nil = NONE
            in f (Array.sub(HashTable, i))
            end
    val _ = (List.app add [
        ("var", T.VAR),
        ("if", T.IF),
        ("then", T.THEN),
        ("else", T.ELSE),
        ("fi", T.FI),
        ("while", T.WHILE),
        ("do", T.DO),
        ("od", T.OD),
        ("procedure", T.PROCEDURE),
        ("print", T.PRINT),
        ("read", T.READ),
        ("call", T.CALL),
        ("inverse", T.INVERSE),
        ("make_rat", T.MAKE_RAT),
        ("rat", T.RAT),
        ("showRat", T.SHOWRAT),
        ("showDecimal", T.SHOWDECIMAL),
        ("fromDecimal", T.FROMDECIMAL),
        ("toDecimal", T.TODECIMAL),
        ("rational", T.RATIONAL_RES),
        ("integer", T.INTEGER_RES),
        ("boolean", T.BOOLEAN_RES)
    ])
end;

open KeyWord;

fun str_to_bigint (num_lst : char list, num_int : BigInt.bigint) =
    case num_lst of
        []  =>  num_int
    |   a::t => str_to_bigint(t,BigInt.add(BigInt.int_to_bigint(ord(a)-ord(#"0")),BigInt.multiply(BigInt.int_to_bigint(10), num_int)))
exception LexerError;

%%

%full
%header (functor Rational_PL0LexFun(structure Tokens: Rational_PL0_TOKENS));
%arg (fileName:string);
%s RATIONAL_PL0 COMMENT;
alpha = [A-Za-z];
digit = [0-9];  
alphanumunder = [A-Za-z0-9_];
whitespace = [\ \t]+;
identifier = {alpha}{alphanumunder}*;
integer = {digit}+;
decimal = {integer}\.{integer};
rational = {integer}\.{digit}*(\({integer}\));
eol = [\n];

%%

<INITIAL>{whitespace}* => (lin:=1; eolpos:=0; YYBEGIN RATIONAL_PL0; continue());
<RATIONAL_PL0>{whitespace}+ => (continue());
<RATIONAL_PL0>{eol} => (lin:=(!lin)+1; eolpos:=yypos+size yytext; continue ());
<RATIONAL_PL0>{rational} => (col:=yypos-(!eolpos); T.RATION(let val x = Rational.fromDecimal(yytext) in x end, !lin, !col));
<RATIONAL_PL0>{decimal} => (col:=yypos-(!eolpos); T.RATION(let val x = Rational.fromDecimal(yytext) in x end, !lin, !col));
<RATIONAL_PL0>{integer} => (col:=yypos-(!eolpos); T.INTEG(let val x = str_to_bigint(explode yytext, BigInt.int_to_bigint(0)) in x end, !lin, !col));
<RATIONAL_PL0>"tt" => (col:=yypos-(!eolpos); T.BOOLEAN (true, !lin, !col));
<RATIONAL_PL0>"ff" => (col:=yypos-(!eolpos); T.BOOLEAN (false, !lin, !col));
<RATIONAL_PL0>"var" => (col:=yypos-(!eolpos); T.VAR(!lin, !col));
<RATIONAL_PL0>"if" => (col:=yypos-(!eolpos); T.IF(!lin, !col));
<RATIONAL_PL0>"then" => (col:=yypos-(!eolpos); T.THEN(!lin, !col));
<RATIONAL_PL0>"else" => (col:=yypos-(!eolpos); T.ELSE(!lin, !col));
<RATIONAL_PL0>"fi" => (col:=yypos-(!eolpos); T.FI(!lin, !col));
<RATIONAL_PL0>"while" => (col:=yypos-(!eolpos); T.WHILE(!lin, !col));
<RATIONAL_PL0>"do" => (col:=yypos-(!eolpos); T.DO(!lin, !col));
<RATIONAL_PL0>"od" => (col:=yypos-(!eolpos); T.OD(!lin, !col));
<RATIONAL_PL0>"procedure" => (col:=yypos-(!eolpos); T.PROCEDURE(!lin, !col));
<RATIONAL_PL0>"print" => (col:=yypos-(!eolpos); T.PRINT(!lin, !col));
<RATIONAL_PL0>"read" => (col:=yypos-(!eolpos); T.READ(!lin, !col));
<RATIONAL_PL0>"call" => (col:=yypos-(!eolpos); T.CALL(!lin, !col));
<RATIONAL_PL0>"inverse" => (col:=yypos-(!eolpos); T.INVERSE(!lin, !col));
<RATIONAL_PL0>"make_rat" => (col:=yypos-(!eolpos); T.MAKE_RAT(!lin, !col));
<RATIONAL_PL0>"rat" => (col:=yypos-(!eolpos); T.RAT(!lin, !col));
<RATIONAL_PL0>"showRat" => (col:=yypos-(!eolpos); T.SHOWRAT(!lin, !col));
<RATIONAL_PL0>"showDecimal" => (col:=yypos-(!eolpos); T.SHOWDECIMAL(!lin, !col));
<RATIONAL_PL0>"fromDecimal" => (col:=yypos-(!eolpos); T.FROMDECIMAL(!lin, !col));
<RATIONAL_PL0>"toDecimal" => (col:=yypos-(!eolpos); T.TODECIMAL(!lin, !col));
<RATIONAL_PL0>"~" => (col:=yypos-(!eolpos); T.MINUS_SIGN(!lin, !col));
<RATIONAL_PL0>"+" => (col:=yypos-(!eolpos); T.PLUS_SIGN(!lin, !col));
<RATIONAL_PL0>"-" => (col:=yypos-(!eolpos); T.SUBTRACT_INT(!lin, !col));
<RATIONAL_PL0>".+." => (col:=yypos-(!eolpos); T.ADD_RAT(!lin, !col));
<RATIONAL_PL0>".-." => (col:=yypos-(!eolpos); T.SUBTRACT_RAT(!lin, !col));
<RATIONAL_PL0>".*." => (col:=yypos-(!eolpos); T.MULTIPLY_RAT(!lin, !col));
<RATIONAL_PL0>"./." => (col:=yypos-(!eolpos); T.DIVIDE_RAT(!lin, !col));
<RATIONAL_PL0>"*" => (col:=yypos-(!eolpos); T.MULTIPLY_INT(!lin, !col));
<RATIONAL_PL0>"/" => (col:=yypos-(!eolpos); T.DIVIDE_INT(!lin, !col));
<RATIONAL_PL0>"%" => (col:=yypos-(!eolpos); T.MOD_INT(!lin, !col));
<RATIONAL_PL0>"!" => (col:=yypos-(!eolpos); T.NEGATE_BOOL(!lin, !col));
<RATIONAL_PL0>"&&" => (col:=yypos-(!eolpos); T.AND_BOOL(!lin, !col));
<RATIONAL_PL0>"||" => (col:=yypos-(!eolpos); T.OR_BOOL(!lin, !col));
<RATIONAL_PL0>"=" => (col:=yypos-(!eolpos); T.EQUAL(!lin, !col));
<RATIONAL_PL0>"<>" => (col:=yypos-(!eolpos); T.NOT_EQUAL(!lin, !col));
<RATIONAL_PL0>"<=" => (col:=yypos-(!eolpos); T.LESS_EQUAL(!lin, !col));
<RATIONAL_PL0>">=" => (col:=yypos-(!eolpos); T.GREATER_EQUAL(!lin, !col));
<RATIONAL_PL0>"<" => (col:=yypos-(!eolpos); T.LESS(!lin, !col));
<RATIONAL_PL0>">" => (col:=yypos-(!eolpos); T.GREATER(!lin, !col));
<RATIONAL_PL0>":=" => (col:=yypos-(!eolpos); T.ASSIGNMENT(!lin, !col));
<RATIONAL_PL0>"(" => (col:=yypos-(!eolpos); T.LPAREN(!lin, !col));
<RATIONAL_PL0>")" => (col:=yypos-(!eolpos); T.RPAREN(!lin, !col));
<RATIONAL_PL0>"{" => (col:=yypos-(!eolpos); T.LCURLYBRACE(!lin, !col));
<RATIONAL_PL0>"}" => (col:=yypos-(!eolpos); T.RCURLYBRACE(!lin, !col));
<RATIONAL_PL0>";" => (col:=yypos-(!eolpos); T.EOS(!lin, !col));
<RATIONAL_PL0>"," => (col:=yypos-(!eolpos); T.COMMA(!lin, !col));
<RATIONAL_PL0>"rational" => (col:=yypos-(!eolpos); T.RATIONAL_RES(!lin, !col));
<RATIONAL_PL0>"integer" => (col:=yypos-(!eolpos); T.INTEGER_RES(!lin, !col));
<RATIONAL_PL0>"boolean" => (col:=yypos-(!eolpos); T.BOOLEAN_RES(!lin, !col));
<RATIONAL_PL0>{identifier} => (col:=yypos-(!eolpos); T.IDENTIFIER(yytext, !lin, !col));
<RATIONAL_PL0>"(*" => (YYBEGIN COMMENT; continue());
<RATIONAL_PL0>. => (col:=yypos-(!eolpos); let val x = "badchar " in badCh(fileName, yytext, !lin, !col) end; T.BAD_CHARACTER(!lin, !col));
<COMMENT>{eol} => (lin:=(!lin)+1; eolpos:=yypos+size yytext; continue());
<COMMENT>"*)" => (YYBEGIN RATIONAL_PL0; continue());
<COMMENT>. => (continue());