(* datatypes.sml *)

signature DATATYPES =
sig
    datatype    Ration = Ration of Rational.rational
    and         Integ = Integ of BigInt.bigint
    and         Boolean = Boolean of bool
    and         CommandSeq = CommandSeq of CommandSeq * Command 
                        |   Empty
    and         Command = Assignment_Cmd of Identifier * Expression
                        |   Call_Cmd of Identifier
                        |   Read_Cmd of Identifier
                        |   Print_Cmd of PrintExp
                        |   Condition_Cmd of Expression * CommandSeq * CommandSeq
                        |   While_Cmd of Expression * CommandSeq
    and         Expression = Exp_Rat of Ration
                        |   Exp_Int of Integ
                        |   Exp_Bool of Boolean
                        |   Exp_Ident of Identifier
                        |   And_Bool of Expression * Expression
                        |   Or_Bool of Expression * Expression
                        |   Neg_Bool of Expression
                        |   Mul_Int of Expression * Expression
                        |   Mul_Rat of Expression * Expression
                        |   Div_Int of Expression * Expression
                        |   Div_Rat of Expression * Expression
                        |   Mod_Int of Expression * Expression
                        |   Add_Int of Expression * Expression
                        |   Sub_Int of Expression * Expression
                        |   Add_Rat of Expression * Expression
                        |   Sub_Rat of Expression * Expression
                        |   Inverse of Expression
                        |   Positive of Expression
                        |   Negative of Expression
                        |   Equal of Expression * Expression
                        |   Not_Equal of Expression * Expression
                        |   Less of Expression * Expression
                        |   Greater of Expression * Expression
                        |   Less_Equal of Expression * Expression
                        |   Greater_Equal of Expression * Expression
                        |   Make_Rat of Expression * Expression
                        |   Rat of Expression
                        |   FromDecimal of Expression
    and         PrintExp = PExp of Expression
                        |   To_Decimal of Expression
                        |   Show_Decimal of Expression
                        |   Show_Rat of Expression
    and         Identifier = Identifier of string
    and         Procedure = Procedure of Identifier * Block
    and         VarDecls = RatDecl of RatVarDecl_Mul
                        |   Declaration_Bool of VarDecls * BoolVarDecl_Mul
                        |   Declaration_intrat of RatVarDecl_Mul * IntVarDecl_Mul
    and         RatVarDecl_Mul = RatVarDecl_Mul of RatVarDecl_Mul * RatVarDecl
                        |   RatVarDecls of RatVarDecl
                        |   Phi
    and         RatVarDecl = RatVarDecl of Identifier
    and         IntVarDecl_Mul = IntVarDecl_Mul of IntVarDecl_Mul * IntVarDecl
                        |   IntVarDecls of IntVarDecl
    and         IntVarDecl = IntVarDecl of Identifier    
    and         BoolVarDecl_Mul = BoolVarDecl_Mul of BoolVarDecl_Mul * BoolVarDecl
                        |   BoolVarDecls of BoolVarDecl
    and         BoolVarDecl = BoolVarDecl of Identifier
    and         DeclSeq = DeclSeq of DeclSeq * Procedure
                        |   DeclSeqVar of VarDecls
    and         Block = Block of DeclSeq * CommandSeq
    and         Rational_PL0 = Program of Block
    and         Result = Result of (Result * Result)
                        |   Null
end;

structure DataTypes : DATATYPES =
struct
    datatype    Ration = Ration of Rational.rational
    and         Integ = Integ of BigInt.bigint
    and         Boolean = Boolean of bool
    and         CommandSeq = CommandSeq of CommandSeq * Command 
                        |   Empty
    and         Command = Assignment_Cmd of Identifier * Expression
                        |   Call_Cmd of Identifier
                        |   Read_Cmd of Identifier
                        |   Print_Cmd of PrintExp
                        |   Condition_Cmd of Expression * CommandSeq * CommandSeq
                        |   While_Cmd of Expression * CommandSeq
    and         Expression = Exp_Rat of Ration
                        |   Exp_Int of Integ
                        |   Exp_Bool of Boolean
                        |   Exp_Ident of Identifier
                        |   And_Bool of Expression * Expression
                        |   Or_Bool of Expression * Expression
                        |   Neg_Bool of Expression
                        |   Mul_Int of Expression * Expression
                        |   Mul_Rat of Expression * Expression
                        |   Div_Int of Expression * Expression
                        |   Div_Rat of Expression * Expression
                        |   Mod_Int of Expression * Expression
                        |   Add_Int of Expression * Expression
                        |   Sub_Int of Expression * Expression
                        |   Add_Rat of Expression * Expression
                        |   Sub_Rat of Expression * Expression
                        |   Inverse of Expression
                        |   Positive of Expression
                        |   Negative of Expression
                        |   Equal of Expression * Expression
                        |   Not_Equal of Expression * Expression
                        |   Less of Expression * Expression
                        |   Greater of Expression * Expression
                        |   Less_Equal of Expression * Expression
                        |   Greater_Equal of Expression * Expression
                        |   Make_Rat of Expression * Expression
                        |   Rat of Expression
                        |   FromDecimal of Expression
    and         PrintExp = PExp of Expression
                        |   To_Decimal of Expression
                        |   Show_Decimal of Expression
                        |   Show_Rat of Expression
    and         Identifier = Identifier of string
    and         Procedure = Procedure of Identifier * Block
    and         VarDecls = RatDecl of RatVarDecl_Mul
                        |   Declaration_Bool of VarDecls * BoolVarDecl_Mul
                        |   Declaration_intrat of RatVarDecl_Mul * IntVarDecl_Mul
    and         RatVarDecl_Mul = RatVarDecl_Mul of RatVarDecl_Mul * RatVarDecl
                        |   RatVarDecls of RatVarDecl
                        |   Phi
    and         RatVarDecl = RatVarDecl of Identifier
    and         IntVarDecl_Mul = IntVarDecl_Mul of IntVarDecl_Mul * IntVarDecl
                        |   IntVarDecls of IntVarDecl
    and         IntVarDecl = IntVarDecl of Identifier    
    and         BoolVarDecl_Mul = BoolVarDecl_Mul of BoolVarDecl_Mul * BoolVarDecl
                        |   BoolVarDecls of BoolVarDecl
    and         BoolVarDecl = BoolVarDecl of Identifier
    and         DeclSeq = DeclSeq of DeclSeq * Procedure
                        |   DeclSeqVar of VarDecls
    and         Block = Block of DeclSeq * CommandSeq
    and         Rational_PL0 = Program of Block
    and         Result = Result of (Result * Result)
                        |   Null
end;