Grammar for parser:
    S = {Program}
    NT = 
        {Program, Block, DeclarationSeq, CommandSeq, ProcDef, VarDecls, BoolVarDecls, VarDecls_intrat, VarDecls_rat, IntVarDecls, RatVarDecls, Multiple_command, command, AssignmentCmd,CallCmd, ReadCmd, PrintCmd, ConditionalCmd, WhileCmd, PrintExpression, Expression, ZeroExpression, FirstExpression, SecondExpression, ThirdExpression, FourthExpression, FifthExpression, SixthExpression}
    T = 
        {;, ,, boolean, integer, rational, procedure, {, }, call, read, print, :=, (, ), if, then, else, fi, while, do, od, .+. ,.-., .*., ./., -, +, inverse ,make_rat, rat, showRat, showDecimal, fromDecimal, toDecimal, Ration, /, *, %, Integ, &&, ||, !, Boolean, =,  <>, <, >, <=, >=, ident, EOF}
    Productions:
        Program             => Block EOF
        Block               => DeclarationSeq CommandSeq
        DeclarationSeq      => DeclarationSeq ProcDef
        DeclarationSeq      => VarDecls
        VarDecls            => VarDecls_intrat BoolVarDecls ;
        VarDecls            => VarDecls_intrat
        VarDecls_intrat     => VarDecls_rat IntVarDecls ;
        VarDecls_intrat     => VarDecls_rat
        VarDecls_rat        => RatVarDecls ;
        VarDecls_rat        => e
        BoolVarDecls        => BoolVarDecls , ident
        BoolVarDecls        => boolean ident
        IntVarDecls         => IntVarDecls , ident
        IntVarDecls         => integer ident
        RatVarDecls         => RatVarDecls , ident
        RatVarDecls         => rational ident
        ProcDef             => procedure ident Block
        CommandSeq          => { Multiple_command }
        Multiple_command    => e
        Multiple_command    => Multiple_command command ;
        command             => AssignmentCmd
        command             => CallCmd
        command             => ReadCmd
        command             => PrintCmd
        command             => ConditionalCmd
        command             => WhileCmd
        AssignmentCmd       => ident := Expression
        CallCmd             => call ident
        ReadCmd             => read( ident )
        PrintCmd            => print( PrintExpression )
        PrintExpression     => Expression
        PrintExpression     => showRat( RatExpression )
        PrintExpression     => showDecimal( RatExpression )
        PrintExpression     => toDecimal( RatExpression )
        Expression          => ZeroExpression
        Expression          => ZeroExpression = ZeroExpression
        Expression          => ZeroExpression <> ZeroExpression
        Expression          => ZeroExpression < ZeroExpression
        Expression          => ZeroExpression > ZeroExpression
        Expression          => ZeroExpression <= ZeroExpression
        Expression          => ZeroExpression >= ZeroExpression
        ZeroExpression      => FirstExpression
        FirstExpression     => FirstExpression && SecondExpression
        FirstExpression     => FirstExpression || SecondExpression
        FirstExpression     => SecondExpression
        SecondExpression    => ! SecondExpression
        SecondExpression    => ThirdExpression
        ThirdExpression     => ThirdExpression + FourthExpression
        ThirdExpression     => ThirdExpression .+. FourthExpression
        ThirdExpression     => ThirdExpression - FourthExpression
        ThirdExpression     => ThirdExpression .-. FourthExpression
        ThirdExpression     => FourthExpression
        FourthExpression    => FourthExpression * FifthExpression
        FourthExpression    => FourthExpression .*. FifthExpression
        FourthExpression    => FourthExpression / FifthExpression
        FourthExpression    => FourthExpression ./. FifthExpression
        FourthExpression    => FourthExpression % FifthExpression
        FourthExpression    => FifthExpression
        FifthExpression     => + FifthExpression
        FifthExpression     => - FifthExpression
        FifthExpression     => inverse FifthExpression
        SixthExpression     => Ration
        SixthExpression     => Integ
        SixthExpression     => Boolean
        SixthExpression     => (expression)
        SixthExpression     => make_rat (expression)
        SixthExpression     => rat (expression)
        SixthExpression     => fromDecimal (expression)
        SixthExpression     => ident

Design Decisions:
    1) Can not declare same variable more than once in same scope.
    2) Can not access a variable if it is not defined.
    3) Can not operate different types of data togethor, i.e. auto type casting is not allowed.
    4) Can not assign procedure in an assignment statement. Can only declare it in ProcDeclaration.
    5) Can not compare two procedures via relational operator.
    6) Can only compare two boolean for equality.
    7) Can not use a variable in an expression if no value is stored in it.
    8) Only tt, ff, integers and rational in decimal normal form are allowed as input.
    9) Rational can not be read of form <integer>., Something must come after decimal point.
    10) Only procedures are callable.
    11) Can oassign value to variables defined in current scope as well as ancestor scopes unless overwritten with another variable of same name. A procedure can not call itself if there is a variable defined in procedure with the same name as of procedure.
    12) For printing inserted spaces at last for easy reading. It can be removed by removing spaces from val result of ex_printexp
    13) Unary operator can be applied multiple times. Their effect will be accumalted. Though this is only in code not fo rinput string. Input string only allows unary postive and unary negative to be used once.


Steps For Execution:
    1) Enter the code in test.rat
    2) Open sml terminal in current directory via: rlwrap sml
    3) CM.make "rational_PL0.cm";
    4) Interpreter.interpret(input_filename, output_filename);

Sources Consulted from Web:
ML-LEX and ML-YACC documentation - https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=43247bea84122c52aa2d86aa86a8f4997825d419