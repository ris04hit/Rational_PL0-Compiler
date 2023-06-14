(* pi.yacc *)
open DataTypes

%%

%name Rational_PL0

%term 
    RATION of Rational.rational | INTEG of BigInt.bigint | BOOLEAN of bool | VAR | IF | THEN | ELSE | FI | WHILE | DO | OD | PROCEDURE | PRINT | READ | CALL | INVERSE | MAKE_RAT | RAT | SHOWRAT | SHOWDECIMAL | FROMDECIMAL | TODECIMAL | MINUS_SIGN | PLUS_SIGN| SUBTRACT_INT | ADD_RAT | SUBTRACT_RAT | MULTIPLY_RAT | DIVIDE_RAT | MULTIPLY_INT | DIVIDE_INT | MOD_INT | NEGATE_BOOL | AND_BOOL | OR_BOOL | EQUAL | NOT_EQUAL | LESS | LESS_EQUAL | GREATER | GREATER_EQUAL | ASSIGNMENT | LPAREN | RPAREN | LCURLYBRACE | RCURLYBRACE | EOS | COMMA | IDENTIFIER of string | COMMENT | EOF | RATIONAL_RES | INTEGER_RES | BOOLEAN_RES | BAD_CHARACTER

%nonterm
    program of Rational_PL0 | block of Block | declarationseq of DeclSeq | commandseq of CommandSeq | procdef of Procedure | vardecls of VarDecls | boolvardecls of BoolVarDecl_Mul | vardecls_intrat of VarDecls | vardecls_rat of RatVarDecl_Mul | intvardecls of IntVarDecl_Mul | ratvardecls of RatVarDecl_Mul | multiple_command of CommandSeq | command of Command | assignmentcmd of Command | callcmd of Command | readcmd of Command | printcmd of Command | conditionalcmd of Command | whilecmd of Command | printexpression of PrintExp | expression of Expression | zeroexpression of Expression | firstexpression of Expression | secondexpression of Expression | thirdexpression of Expression | fourthexpression of Expression | fifthexpression of Expression | sixthexpression of Expression

%pos int
%eop EOF
%noshift EOF EOS
%left ADD_RAT SUBTRACT_RAT
%nodefault
%verbose
%keyword VAR IF THEN ELSE FI WHILE DO OD PROCEDURE PRINT READ CALL INVERSE MAKE_RAT RAT SHOWRAT SHOWDECIMAL FROMDECIMAL TODECIMAL RATIONAL_RES INTEGER_RES BOOLEAN_RES
%start program
%arg (filename) : string

%%  

program:            block EOF                                           ((Program (block)))
block:              declarationseq commandseq                           ((Block (declarationseq, commandseq)))
declarationseq:     declarationseq procdef EOS                          ((DeclSeq (declarationseq, procdef)))
declarationseq:     vardecls                                            ((DeclSeqVar (vardecls)))
vardecls:           vardecls_intrat boolvardecls EOS                    ((Declaration_Bool (vardecls_intrat, boolvardecls)))
vardecls:           vardecls_intrat                                     ((vardecls_intrat))
vardecls_intrat:    vardecls_rat intvardecls EOS                        ((Declaration_intrat (vardecls_rat, intvardecls)))
vardecls_intrat:    vardecls_rat                                        ((RatDecl(vardecls_rat)))
vardecls_rat:       ratvardecls EOS                                     ((ratvardecls))
vardecls_rat:                                                           ((Phi))
boolvardecls:       boolvardecls COMMA IDENTIFIER                       ((BoolVarDecl_Mul (boolvardecls, BoolVarDecl (Identifier (IDENTIFIER)))))
boolvardecls:       BOOLEAN_RES IDENTIFIER                              ((BoolVarDecls (BoolVarDecl (Identifier (IDENTIFIER)))))
intvardecls:        intvardecls COMMA IDENTIFIER                        ((IntVarDecl_Mul (intvardecls, IntVarDecl (Identifier (IDENTIFIER)))))
intvardecls:        INTEGER_RES IDENTIFIER                              ((IntVarDecls (IntVarDecl (Identifier (IDENTIFIER)))))
ratvardecls:        ratvardecls COMMA IDENTIFIER                        ((RatVarDecl_Mul (ratvardecls, RatVarDecl (Identifier (IDENTIFIER)))))
ratvardecls:        RATIONAL_RES IDENTIFIER                             ((RatVarDecls ( RatVarDecl (Identifier (IDENTIFIER)))))
procdef:            PROCEDURE IDENTIFIER block                          ((Procedure (Identifier (IDENTIFIER), block)))
commandseq:         LCURLYBRACE multiple_command RCURLYBRACE            ((multiple_command))
multiple_command:                                                       ((Empty))
multiple_command:   multiple_command command EOS                        ((CommandSeq (multiple_command, command)))
command:            assignmentcmd                                       ((assignmentcmd))
command:            callcmd                                             ((callcmd))
command:            readcmd                                             ((readcmd))
command:            printcmd                                            ((printcmd))
command:            conditionalcmd                                      ((conditionalcmd))
command:            whilecmd                                            ((whilecmd))
assignmentcmd:      IDENTIFIER ASSIGNMENT expression                    ((Assignment_Cmd (Identifier (IDENTIFIER), expression)))
callcmd:            CALL IDENTIFIER                                     ((Call_Cmd (Identifier (IDENTIFIER))))
readcmd:            READ LPAREN IDENTIFIER RPAREN                       ((Read_Cmd (Identifier (IDENTIFIER))))
conditionalcmd:     IF expression THEN commandseq ELSE commandseq FI    ((Condition_Cmd (expression, commandseq1, commandseq2)))
whilecmd:           WHILE expression DO commandseq OD                   ((While_Cmd (expression, commandseq)))
printcmd:           PRINT LPAREN printexpression RPAREN                 ((Print_Cmd (printexpression)))
printexpression:    expression                                          ((PExp (expression)))
printexpression:    SHOWRAT LPAREN expression RPAREN                    ((Show_Rat (expression)))
printexpression:    SHOWDECIMAL LPAREN expression RPAREN                ((Show_Decimal (expression)))
printexpression:    TODECIMAL LPAREN expression RPAREN                  ((To_Decimal (expression)))
expression:         zeroexpression                                      ((zeroexpression))
expression:         zeroexpression EQUAL zeroexpression                 ((Equal (zeroexpression1, zeroexpression2)))
expression:         zeroexpression NOT_EQUAL zeroexpression             ((Not_Equal (zeroexpression1, zeroexpression2)))
expression:         zeroexpression LESS zeroexpression                  ((Less (zeroexpression1, zeroexpression2)))
expression:         zeroexpression GREATER zeroexpression               ((Greater (zeroexpression1, zeroexpression2)))
expression:         zeroexpression LESS_EQUAL zeroexpression            ((Less_Equal (zeroexpression1, zeroexpression2)))
expression:         zeroexpression GREATER_EQUAL zeroexpression         ((Greater_Equal (zeroexpression1, zeroexpression2)))
zeroexpression:     firstexpression                                     ((firstexpression))
firstexpression:    firstexpression AND_BOOL secondexpression           ((And_Bool (firstexpression, secondexpression)))
firstexpression:    firstexpression OR_BOOL secondexpression            ((Or_Bool (firstexpression, secondexpression)))
firstexpression:    secondexpression                                    ((secondexpression))
secondexpression:   NEGATE_BOOL secondexpression                        ((Neg_Bool (secondexpression)))
secondexpression:   thirdexpression                                     ((thirdexpression))
thirdexpression:    thirdexpression PLUS_SIGN fourthexpression          ((Add_Int (thirdexpression, fourthexpression)))
thirdexpression:    thirdexpression ADD_RAT fourthexpression            ((Add_Rat (thirdexpression, fourthexpression)))
thirdexpression:    thirdexpression SUBTRACT_INT fourthexpression       ((Sub_Int (thirdexpression, fourthexpression)))
thirdexpression:    thirdexpression SUBTRACT_RAT fourthexpression       ((Sub_Rat (thirdexpression, fourthexpression)))
thirdexpression:    fourthexpression                                    ((fourthexpression))
fourthexpression:   fourthexpression MULTIPLY_INT fifthexpression       ((Mul_Int (fourthexpression, fifthexpression)))
fourthexpression:   fourthexpression MULTIPLY_RAT fifthexpression       ((Mul_Rat (fourthexpression, fifthexpression)))
fourthexpression:   fourthexpression DIVIDE_INT fifthexpression         ((Div_Int (fourthexpression, fifthexpression)))
fourthexpression:   fourthexpression DIVIDE_RAT fifthexpression         ((Div_Rat (fourthexpression, fifthexpression)))
fourthexpression:   fourthexpression MOD_INT fifthexpression            ((Mod_Int (fourthexpression, fifthexpression)))
fourthexpression:   fifthexpression                                     ((fifthexpression))
fifthexpression:    PLUS_SIGN fifthexpression                           ((Positive (fifthexpression)))
fifthexpression:    MINUS_SIGN fifthexpression                          ((Negative (fifthexpression)))
fifthexpression:    INVERSE fifthexpression                             ((Inverse (fifthexpression)))
fifthexpression:    sixthexpression                                     ((sixthexpression))
sixthexpression:    RATION                                              ((Exp_Rat ( Ration (RATION))))
sixthexpression:    INTEG                                               ((Exp_Int ( Integ (INTEG))))
sixthexpression:    BOOLEAN                                             ((Exp_Bool ( Boolean (BOOLEAN))))
sixthexpression:    LPAREN expression RPAREN                            ((expression))
sixthexpression:    MAKE_RAT LPAREN expression COMMA expression RPAREN  ((Make_Rat (expression1, expression2)))
sixthexpression:    RAT LPAREN expression RPAREN                        ((Rat (expression)))
sixthexpression:    FROMDECIMAL LPAREN expression RPAREN                ((FromDecimal (expression)))
sixthexpression:    IDENTIFIER                                          ((Exp_Ident (Identifier (IDENTIFIER))))