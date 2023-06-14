(* compiler.sml *)
(* CM.make "rational_PL0.cm; *)

structure Rational_PL0 :
sig val compile : string -> DataTypes.Rational_PL0
end =
struct
exception Rational_PL0Error;
fun compile (fileName) =
    let val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn
            n => if TextIO.endOfStream inStream
                then ""
                else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
            (msg,line,col) =>
            print (fileName^"["^Int.toString line^":"
                ^Int.toString col^"] "^msg^"");
        val _ = Control.Print.printDepth:=25;
        val (tree,rem) = Rational_PL0Parser.parse
                    (15,
                    (Rational_PL0Parser.makeLexer grab fileName),
                    printError,
                    fileName)
            handle Rational_PL0Parser.ParseError => raise Rational_PL0Error;
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;
    in
     tree
    end
end;



signature INTERPRETER =
sig
    val interpret: string * string -> 'a option
end;

structure Interpreter: INTERPRETER =
struct
    exception MultipleDeclaration
    exception VariableNotDefined
    exception TypeMisMatch
    exception ProcedureNotAssignable
    exception ProcedureNotComparable
    exception BooleanNotComparable
    exception ValueNotAvailable
    exception InvalidInput
    exception DatatypeNotCallable

    val rat_var : (((string, Rational.rational option) HashTable.hash_table * int list ) list) ref = ref []
    val int_var : (((string, BigInt.bigint option) HashTable.hash_table * int list ) list) ref = ref []
    val bool_var : (((string, bool option) HashTable.hash_table * int list ) list) ref = ref []
    val proc_var : (((string, (DataTypes.CommandSeq * int list) option) HashTable.hash_table * int list ) list) ref = ref []
    val rat_scope : (((string, Rational.rational option) HashTable.hash_table * int list ) list) ref = ref [(HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found"), [0])]
    val int_scope : (((string, BigInt.bigint option) HashTable.hash_table * int list ) list) ref = ref [(HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found"), [0])]
    val bool_scope : (((string, bool option) HashTable.hash_table * int list ) list) ref = ref [(HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found"), [0])]
    val proc_scope : (((string,( DataTypes.CommandSeq * int list) option) HashTable.hash_table * int list ) list) ref = ref [(HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found"), [0])]
    val current_scope : (int list) ref = ref [0]
    val scope_list : ((int list) list) ref = ref [[0]]
    val output_data : string ref = ref ""

    (* Parses file <filename> *)
    fun parse(filename) = Rational_PL0.compile(filename)

    (* Restores all hash map to empty data structures *)
    fun restore_var() =
        let
            val _ = rat_var := [(HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found"), [0])]
            val _ = int_var := [(HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found"), [0])]
            val _ = bool_var := [(HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found"), [0])]
            val _ = proc_var := [(HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found"), [0])]
            val _ = rat_scope := [(HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found"), [0])]
            val _ = int_scope := [(HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found"), [0])]
            val _ = bool_scope := [(HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found"), [0])]
            val _ = proc_scope := [(HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found"), [0])]
            val _ = current_scope := [0]
            val _ = scope_list := [[0]]
            val _ = output_data := ""
        in
            NONE
        end
    
    (* Raises Exception with error_message message*)
    fun raise_exception (e , message : string) =
        let
            val _ = print("\nError: "^message)
            val  _ = restore_var()
        in
            raise e
        end

    (* Compares two list for equality *)
    fun lst_comp (lst1 : int list, lst2 : int list) =
        case (lst1, lst2) of
            ([], [])    => true
        |   ([], _)     => false
        |   (_, [])     => false
        |   (h1::t1, h2::t2) => ((h1 = h2) andalso lst_comp(t1, t2))

    (* Checks child-ancestor relation for list *)
    fun ancestor (child : int list, parent : int list) =
        let
            val revchild = List.rev child
            val revparent = List.rev parent
        in
            case (revchild, revparent) of
                (_, [])        => true
            |   ([], _)        => false
            |   (h1::t1, h2::t2) => ((h1=h2) andalso ancestor(List.rev t1, List.rev t2))
        end

    (* Creates new scope *)
    fun create_scope (curr_scope : int list, scope : (int list) list) =
        let
            exception UnexpectedException
        in
            case scope of 
                []  =>  1::curr_scope
            |   h::t => if (((List.length h) = (List.length curr_scope) + 1) andalso (ancestor(h, curr_scope)))
                            then case h of x::y => (x+1)::curr_scope | _ => raise UnexpectedException
                        else create_scope(curr_scope, t)
        end

    (* Searches for a variable in var list and returns whether found or not, its value otherwise NONE *)
    fun value_var (scope : ((string, 'a option) HashTable.hash_table * int list) list, var : string) =
        case scope of
            []      =>  (false, NONE, [])
        |   h::t    =>  if ((ancestor(!current_scope, #2 h)) andalso (HashTable.inDomain(#1 h)(var)))
                            then (true, HashTable.lookup(#1 h)(var), #2 h)
                            else value_var(t, var)
    
    (* Searches for given scope in <datatyoe>_scope *)
    fun search_scope(new_scope : int list, lst_scope : ((string, 'a option) HashTable.hash_table * int list) list) =
        case lst_scope of
            []      =>  (HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found"), [0])
        |   h::t    =>  if lst_comp((#2 h),new_scope) then (h) else search_scope(new_scope, t)

    (* Updates the value of a variable in current or ancestor scope if variable exists *)
    fun update (scope : ((string, 'a option) HashTable.hash_table * int list) list, var : string, value : 'a) =
        case scope of
            []      => []
        |   h::t    => if ((ancestor(!current_scope, #2 h)) andalso (HashTable.inDomain(#1 h)(var)))
                            then let val _ = HashTable.insert(#1 h)(var, SOME value) in h::t end
                            else h::update(t, var, value)
    
    (* Inserts procedure in proc_scope *)
    fun scope_insert (scope : (((string, 'a option) HashTable.hash_table * (int list) ) list), var : string, value : 'a) =
        case scope of
            []      => []
        |   h::t    => if lst_comp((#2 h),!current_scope)
                            then let val _ = HashTable.insert(#1 h)(var, SOME value) in h::t end
                            else h::scope_insert(t, var, value)


    (* Searches for a variable in all var lists current scope and returns true if present in any else false*)
    fun search (var : string) =
        let
            val ratv = (!rat_scope)
            val intv = (!int_scope)
            val boolv = (!bool_scope)
            val procv = (!proc_scope)
        in
             (* HashTable.inDomain(#1 ratv)(var) orelse  HashTable.inDomain(#1 intv)(var) orelse  HashTable.inDomain(#1 boolv)(var) orelse HashTable.inDomain(#1 procv)(var) *)
             (#1 (value_var(ratv, var))) orelse (#1 (value_var(intv, var))) orelse (#1 (value_var(boolv, var))) orelse (#1 (value_var(procv, var)))
        end
    
    (* Function to declare variable *)
    fun var_identifier(identifier : DataTypes.Identifier, var_list : ((string, 'a option) HashTable.hash_table * int list) list ref) =
        let
            val var = case identifier of DataTypes.Identifier(x) => x
            val var_list_dummy = !var_list
            fun var_insert (scope : (((string, 'a option) HashTable.hash_table * (int list) ) list), var : string) =
                case scope of
                    []      => []
                |   h::t    => if lst_comp((#2 h),!current_scope)
                                    then 
                                        if HashTable.inDomain(#1 h)(var) 
                                            then  raise_exception (MultipleDeclaration, "variable "^var^" declared multiple times") 
                                        else
                                            let val _ = HashTable.insert(#1 h)(var, NONE) in h::t end
                                else h::var_insert(t, var)
            val var_list_dummy = var_insert(var_list_dummy, var)
            val _ = var_list := var_list_dummy
        in
            DataTypes.Null
        end

    (* Function to get Rational.rational from Ration *)
    fun getrat (rat : DataTypes.Ration) =
        case rat of
            DataTypes.Ration(ration) => ration
    
    (* Function to get BigInt.bigint from Integ *)
    fun getint (inte : DataTypes.Integ) =
        case inte of
            DataTypes.Integ(integ) => integ

    (* Function to get bool from Boolean *)
    fun getbool (boo : DataTypes.Boolean) =
        case boo of 
            DataTypes.Boolean(boolean) => boolean

    (* Function to get string from Identifier *)
    fun getident (ident : DataTypes.Identifier) =
        case ident of
            DataTypes.Identifier(identifier) => identifier

    (* Functions for executing different nodes of AST *) 

    fun ex_int_expression (expression : DataTypes.Expression) =
        let
            val rat_var_cp = !rat_var
            val int_var_cp = !int_var
            val bool_var_cp = !bool_var
            val proc_var_cp = !proc_var
            val result =
                case expression of
                    DataTypes.Exp_Int (integ) => getint(integ)
                |   DataTypes.Exp_Ident (identifier) =>
                        let 
                            val ident_str = getident(identifier)
                            val (rat_cond , val_dummy, sc_rat) = (value_var(rat_var_cp, ident_str))
                            val (int_cond , value_int, sc_int) = (value_var(int_var_cp, ident_str))
                            val (bool_cond, val_dummy, sc_bool) = (value_var(bool_var_cp, ident_str))
                            val (proc_cond, val_dummy, sc_proc) = (value_var(proc_var_cp, ident_str))
                            fun maxm(lst : (bool * int list) list) =
                                case lst of
                                    []  => 0
                                |   h::t => if (#1 h) then (if (List.length(#2 h) > maxm(t)) then List.length(#2 h) else maxm(t)) else maxm(t)
                            val max_len = maxm([(rat_cond, sc_rat), (int_cond, sc_int), (bool_cond, sc_bool), (proc_cond, sc_proc)])
                        in
                            if not ((rat_cond andalso (List.length(sc_rat) = max_len)) orelse (int_cond andalso (List.length(sc_int) = max_len)) orelse (bool_cond andalso (List.length(sc_bool) = max_len)) orelse (proc_cond andalso (List.length(sc_proc) = max_len)))
                                then raise_exception(VariableNotDefined, "Variable "^ident_str^" is not defined")
                            else if (int_cond andalso (List.length(sc_int) = max_len)) then case (value_int, sc_int) of
                                (NONE, sc) => raise_exception(ValueNotAvailable, "No Value stored in variable "^ident_str)
                            |   (value_of_var, sc) => valOf(value_of_var) 
                                else raise_exception(TypeMisMatch, "Variable "^ident_str^" is not registered as integer")
                        end
                |   DataTypes.Mul_Int (expression1, expression2) => 
                        BigInt.multiply(ex_int_expression(expression1), ex_int_expression(expression2))
                |   DataTypes.Div_Int (expression1, expression2) =>
                        BigInt.divide(ex_int_expression(expression2), ex_int_expression(expression1))
                |   DataTypes.Mod_Int (expression1, expression2) =>
                        BigInt.modulo(ex_int_expression(expression2), ex_int_expression(expression1))
                |   DataTypes.Add_Int (expression1, expression2) =>
                        BigInt.add(ex_int_expression(expression1), ex_int_expression(expression2))
                |   DataTypes.Sub_Int (expression1, expression2) =>
                        BigInt.subtract(ex_int_expression(expression1), ex_int_expression(expression2))
                |   DataTypes.Positive (expression) => ex_int_expression(expression)
                |   DataTypes.Negative (expression) =>
                        BigInt.multiply(ex_int_expression(expression), BigInt.int_to_bigint(~1))
                |   _ => raise_exception(TypeMisMatch, "Expression is not registered as integer")
        in
            result
        end
                

    and ex_rat_expression (expression : DataTypes.Expression) =
        let
            val rat_var_cp = !rat_var
            val int_var_cp = !int_var
            val bool_var_cp = !bool_var
            val proc_var_cp = !proc_var
            val result =
                case expression of
                    DataTypes.Exp_Rat (ration) => getrat(ration)
                |   DataTypes.Exp_Ident (identifier) =>
                        let 
                            val ident_str = getident(identifier)
                            val (rat_cond , value_rat, sc_rat) = (value_var(rat_var_cp, ident_str))
                            val (int_cond , value_int, sc_int) = (value_var(int_var_cp, ident_str))
                            val (bool_cond, val_dummy, sc_bool) = (value_var(bool_var_cp, ident_str))
                            val (proc_cond, val_dummy, sc_proc) = (value_var(proc_var_cp, ident_str))
                            fun maxm(lst : (bool * int list) list) =
                                case lst of
                                    []  => 0
                                |   h::t => if (#1 h) then (if (List.length(#2 h) > maxm(t)) then List.length(#2 h) else maxm(t)) else maxm(t)
                            val max_len = maxm([(rat_cond, sc_rat), (int_cond, sc_int), (bool_cond, sc_bool), (proc_cond, sc_proc)])
                        in
                            if not ((rat_cond andalso (List.length(sc_rat) = max_len)) orelse (int_cond andalso (List.length(sc_int) = max_len)) orelse (bool_cond andalso (List.length(sc_bool) = max_len)) orelse (proc_cond andalso (List.length(sc_proc) = max_len)))
                                then raise_exception(VariableNotDefined, "Variable "^ident_str^" is not defined")
                            else if (rat_cond andalso (List.length(sc_rat) = max_len)) then case (value_rat, sc_rat) of
                                (NONE, sc) => raise_exception(ValueNotAvailable, "No Value stored in variable "^ident_str)
                            |   (value_of_var, sc) => valOf(value_of_var) 
                                else raise_exception(TypeMisMatch, "Variable "^ident_str^" is not registered as rational")
                        end
                |   DataTypes.Mul_Rat(expression1, expression2) => 
                        Rational.multiply(ex_rat_expression(expression1), ex_rat_expression(expression2))
                |   DataTypes.Div_Rat(expression1, expression2) =>
                        valOf(Rational.divide(ex_rat_expression(expression1),ex_rat_expression(expression2)))
                |   DataTypes.Add_Rat(expression1, expression2) =>
                        Rational.add(ex_rat_expression(expression1), ex_rat_expression(expression2))
                |   DataTypes.Sub_Rat(expression1, expression2) =>
                        Rational.subtract(ex_rat_expression(expression1), ex_rat_expression(expression2))
                |   DataTypes.Inverse(expression) => valOf(Rational.inverse(ex_rat_expression(expression)))
                |   DataTypes.Positive(expression) => ex_rat_expression(expression)
                |   DataTypes.Negative(expression) => Rational.neg(ex_rat_expression(expression))
                |   DataTypes.Make_Rat(expression1, expression2) => 
                        valOf(Rational.make_rat(ex_int_expression(expression1), ex_int_expression(expression2)))
                |   DataTypes.Rat(expression) => valOf(Rational.rat(ex_int_expression(expression)))
                |   DataTypes.FromDecimal(expression) => ex_rat_expression(expression)
                |   _ => raise_exception(TypeMisMatch, "Expression is not rational")
        in
            result
        end
    and ex_bool_expression (expression : DataTypes.Expression) =
        let
            val rat_var_cp = !rat_var
            val int_var_cp = !int_var
            val bool_var_cp = !bool_var
            val proc_var_cp = !proc_var
            fun compare_help (expression1, expression2, rat_com, int_com, bool_com) =
                case expression1 of
                    DataTypes.Exp_Rat(a) => rat_com (getrat (a), ex_rat_expression(expression2))
                |   DataTypes.Exp_Int(a) => int_com (getint (a), ex_int_expression(expression2))
                |   DataTypes.Exp_Bool(a) => bool_com (getbool (a), ex_bool_expression(expression2))
                |   DataTypes.Exp_Ident(a) => 
                        let
                            val ident_str = getident(a)
                            val (rat_cond , value_rat, sc_rat) = (value_var(rat_var_cp, ident_str))
                            val (int_cond , value_int, sc_int) = (value_var(int_var_cp, ident_str))
                            val (bool_cond, value_bool, sc_bool) = (value_var(bool_var_cp, ident_str))
                            val (proc_cond, val_dummy, sc_proc) = (value_var(proc_var_cp, ident_str))
                            fun maxm(lst : (bool * int list) list) =
                                case lst of
                                    []  => 0
                                |   h::t => if (#1 h) then (if (List.length(#2 h) > maxm(t)) then List.length(#2 h) else maxm(t)) else maxm(t)
                            val max_len = maxm([(rat_cond, sc_rat), (int_cond, sc_int), (bool_cond, sc_bool), (proc_cond, sc_proc)])
                            val res =
                                    if (rat_cond) andalso (List.length(sc_rat)=max_len)
                                        then case (value_rat) of
                                            NONE => raise_exception(ValueNotAvailable, "No Value stored in variable "^ident_str)
                                        |   _ => rat_com(valOf(value_rat), ex_rat_expression(expression2))
                                    else if (int_cond) andalso (List.length(sc_int) = max_len)
                                        then case (value_int) of
                                            NONE => raise_exception(ValueNotAvailable, "No Value stored in variable "^ident_str)
                                        |   _ => int_com(valOf(value_int), ex_int_expression(expression2))
                                    else if (bool_cond) andalso (List.length(sc_bool) = max_len)
                                        then if (value_bool) = NONE 
                                            then raise_exception(ValueNotAvailable, "No Value stored in variable "^ident_str)
                                            else bool_com(valOf(value_bool), ex_bool_expression(expression2))
                                    else if (proc_cond) andalso (List.length(sc_proc) = max_len)
                                        then raise_exception (ProcedureNotComparable, "Can not compare procedure")
                                    else
                                        raise_exception (VariableNotDefined, "Variable "^ident_str^" is not defined")
                        in
                            res
                        end
                |   DataTypes.And_Bool(a,b) => bool_com (ex_bool_expression(expression1), ex_bool_expression(expression2))
                |   DataTypes.Or_Bool(a,b) => bool_com (ex_bool_expression(expression1), ex_bool_expression(expression2))
                |   DataTypes.Neg_Bool(a) => bool_com (ex_bool_expression(expression1), ex_bool_expression(expression2))
                |   DataTypes.Mul_Int(a,b) => int_com (ex_int_expression(expression1), ex_int_expression(expression2))
                |   DataTypes.Mul_Rat(a,b) => rat_com (ex_rat_expression(expression1), ex_rat_expression(expression2))
                |   DataTypes.Div_Int(a,b) => int_com (ex_int_expression(expression1), ex_int_expression(expression2))
                |   DataTypes.Div_Rat(a,b) => rat_com (ex_rat_expression(expression1), ex_rat_expression(expression2))
                |   DataTypes.Mod_Int(a,b) => int_com (ex_int_expression(expression1), ex_int_expression(expression2))
                |   DataTypes.Add_Int(a,b) => int_com (ex_int_expression(expression1), ex_int_expression(expression2))
                |   DataTypes.Sub_Int(a,b) => int_com (ex_int_expression(expression1), ex_int_expression(expression2))
                |   DataTypes.Add_Rat(a,b) => rat_com (ex_rat_expression(expression1), ex_rat_expression(expression2))
                |   DataTypes.Sub_Rat(a,b) => rat_com (ex_rat_expression(expression1), ex_rat_expression(expression2))
                |   DataTypes.Inverse(a) => rat_com (ex_rat_expression(expression1), ex_rat_expression(expression2))
                |   DataTypes.Positive(a) => compare_help (a, expression2, rat_com, int_com, bool_com)
                |   DataTypes.Negative(a) => compare_help (a, expression2, rat_com, int_com, bool_com)
                |   DataTypes.Equal(a,b) => bool_com (ex_bool_expression(expression1), ex_bool_expression(expression2))
                |   DataTypes.Not_Equal(a,b) => bool_com (ex_bool_expression(expression1), ex_bool_expression(expression2))
                |   DataTypes.Less(a,b) => bool_com (ex_bool_expression(expression1), ex_bool_expression(expression2))
                |   DataTypes.Greater(a,b) => bool_com (ex_bool_expression(expression1), ex_bool_expression(expression2))
                |   DataTypes.Less_Equal(a,b)=> bool_com (ex_bool_expression(expression1), ex_bool_expression(expression2))
                |   DataTypes.Greater_Equal(a,b) => bool_com (ex_bool_expression(expression1), ex_bool_expression(expression2))
                |   DataTypes.Make_Rat(a,b) => rat_com (ex_rat_expression(expression1), ex_rat_expression(expression2))
                |   DataTypes.Rat(a) => rat_com (ex_rat_expression(expression1), ex_rat_expression(expression2))
                |   DataTypes.FromDecimal(a) => rat_com (ex_rat_expression(expression1), ex_rat_expression(expression2))
            val result =
                case expression of
                    DataTypes.Exp_Bool (boolean) => getbool(boolean)
                |   DataTypes.Exp_Ident (identifier) =>
                        let 
                            val ident_str = getident(identifier)
                            val (rat_cond , value_rat, sc_rat) = (value_var(rat_var_cp, ident_str))
                            val (int_cond , value_int, sc_int) = (value_var(int_var_cp, ident_str))
                            val (bool_cond, value_bool, sc_bool) = (value_var(bool_var_cp, ident_str))
                            val (proc_cond, val_dummy, sc_proc) = (value_var(proc_var_cp, ident_str))
                            fun maxm(lst : (bool * int list) list) =
                                case lst of
                                    []  => 0
                                |   h::t => if (#1 h) then (if (List.length(#2 h) > maxm(t)) then List.length(#2 h) else maxm(t)) else maxm(t)
                            val max_len = maxm([(rat_cond, sc_rat), (int_cond, sc_int), (bool_cond, sc_bool), (proc_cond, sc_proc)])
                        in
                            if not ((rat_cond andalso (List.length(sc_rat) = max_len)) orelse (int_cond andalso (List.length(sc_int) = max_len)) orelse (bool_cond andalso (List.length(sc_bool) = max_len)) orelse (proc_cond andalso (List.length(sc_proc) = max_len)))
                                then raise_exception(VariableNotDefined, "Variable "^ident_str^" is not defined")
                            else if (bool_cond andalso (List.length(sc_bool) = max_len)) then case (value_bool, sc_bool) of
                                (NONE, sc) => raise_exception(ValueNotAvailable, "No Value stored in variable "^ident_str)
                            |   (value_of_var, sc) => valOf(value_of_var) 
                                else raise_exception(TypeMisMatch, "Variable "^ident_str^" is not registered as rational")
                        end
                |   DataTypes.And_Bool (expression1, expression2) =>
                        ex_bool_expression(expression1) andalso ex_bool_expression(expression2)
                |   DataTypes.Or_Bool (expression1, expression2) =>
                        ex_bool_expression(expression1) orelse ex_bool_expression(expression2)
                |   DataTypes.Neg_Bool (expression) => not (ex_bool_expression(expression))
                |   DataTypes.Equal (expression1, expression2) =>
                        let
                            fun rat_com (exp1, exp2) = Rational.equal(exp1 ,exp2)
                            fun int_com (exp1, exp2) = (BigInt.compare(exp1,exp2) = 0)
                            fun bool_com (exp1, exp2) = (exp1 = exp2)
                        in
                            compare_help(expression1, expression2, rat_com, int_com, bool_com)
                        end
                |   DataTypes.Not_Equal (expression1, expression2) =>
                        let
                            fun rat_com (exp1, exp2) = not (Rational.equal(exp1 ,exp2))
                            fun int_com (exp1, exp2) = not (BigInt.compare(exp1,exp2) = 0)
                            fun bool_com (exp1, exp2) = not (exp1 = exp2)
                        in
                            compare_help(expression1, expression2, rat_com, int_com, bool_com)
                        end
                |   DataTypes.Less (expression1, expression2) =>
                        let
                            fun rat_com (exp1, exp2) = Rational.less(exp1 ,exp2)
                            fun int_com (exp1, exp2) = (BigInt.compare(exp1,exp2) = ~1)
                            fun bool_com (exp1, exp2) = raise_exception(BooleanNotComparable, "Boolean can only be compared for equality")
                        in
                            compare_help(expression1, expression2, rat_com, int_com, bool_com)
                        end
                |   DataTypes.Greater (expression1, expression2) =>
                        let
                            fun rat_com (exp1, exp2) = not (Rational.equal(exp1 ,exp2) orelse (Rational.less(exp1, exp2)))
                            fun int_com (exp1, exp2) = (BigInt.compare(exp1,exp2) = 1)
                            fun bool_com (exp1, exp2) = raise_exception(BooleanNotComparable, "Boolean can only be compared for equality")
                        in
                            compare_help(expression1, expression2, rat_com, int_com, bool_com)
                        end
                |   DataTypes.Less_Equal (expression1, expression2) =>
                        let
                            fun rat_com (exp1, exp2) = Rational.equal(exp1 ,exp2) orelse (Rational.less(exp1, exp2))
                            fun int_com (exp1, exp2) = not (BigInt.compare(exp1,exp2) = 1)
                            fun bool_com (exp1, exp2) = raise_exception(BooleanNotComparable, "Boolean can only be compared for equality")
                        in
                            compare_help(expression1, expression2, rat_com, int_com, bool_com)
                        end
                |   DataTypes.Greater_Equal (expression1, expression2) =>
                        let
                            fun rat_com (exp1, exp2) = not (Rational.less(exp1 ,exp2))
                            fun int_com (exp1, exp2) = not (BigInt.compare(exp1,exp2) = ~1)
                            fun bool_com (exp1, exp2) = raise_exception(BooleanNotComparable, "Boolean can only be compared for equality")
                        in
                            compare_help(expression1, expression2, rat_com, int_com, bool_com)
                        end
                |   _ => raise_exception(TypeMisMatch, "Expression is not boolean")
        in
            result
        end

    and ex_printexp (printexp : DataTypes.PrintExp) =
        let
            val rat_var_cp = !rat_var
            val int_var_cp = !int_var
            val bool_var_cp = !bool_var
            val proc_var_cp = !proc_var
            fun fun_rat(expression) = Rational.showDecimal(ex_rat_expression(expression))
            fun fun_int(expression) = BigInt.show_int(ex_int_expression(expression))
            fun fun_bool(expression) = if ex_bool_expression(expression) then "tt" else "ff"
            fun helper_expression(expression : DataTypes.Expression) =
                case expression of
                    DataTypes.Exp_Rat(a) => fun_rat(expression)
                |   DataTypes.Exp_Int(a) => fun_int(expression)
                |   DataTypes.Exp_Bool(a) => fun_bool(expression)
                |   DataTypes.Exp_Ident(a) =>  
                            let
                                val ident_str = getident(a)
                            val (rat_cond , value_rat, sc_rat) = (value_var(rat_var_cp, ident_str))
                            val (int_cond , value_int, sc_int) = (value_var(int_var_cp, ident_str))
                            val (bool_cond, value_bool, sc_bool) = (value_var(bool_var_cp, ident_str))
                            val (proc_cond, value_proc, sc_proc) = (value_var(proc_var_cp, ident_str))
                            fun maxm(lst : (bool * int list) list) =
                                case lst of
                                    []  => 0
                                |   h::t => if (#1 h) then (if (List.length(#2 h) > maxm(t)) then List.length(#2 h) else maxm(t)) else maxm(t)
                            val max_len = maxm([(rat_cond, sc_rat), (int_cond, sc_int), (bool_cond, sc_bool), (proc_cond, sc_proc)])
                                val res =
                                        if (rat_cond andalso (List.length(sc_rat) = max_len))
                                            then case (value_rat) of
                                                NONE => raise_exception(ValueNotAvailable, "No Value stored in variable "^ident_str)
                                            |   _ => fun_rat(expression)
                                        else if (int_cond andalso (List.length(sc_int) = max_len))
                                            then case (value_int) of
                                                NONE => raise_exception(ValueNotAvailable, "No Value stored in variable "^ident_str)
                                            |   _ => fun_int(expression)
                                        else if (bool_cond andalso (List.length(sc_bool) = max_len))
                                            then if (value_bool) = NONE 
                                                then raise_exception(ValueNotAvailable, "No Value stored in variable "^ident_str)
                                                else fun_bool(expression)
                                        else if (proc_cond andalso (List.length(sc_proc) = max_len))
                                            then raise_exception (ProcedureNotComparable, "Can not compare procedure")
                                        else
                                            raise_exception (VariableNotDefined, "Variable "^ident_str^" is not defined")
                            in
                                res
                            end
                |   DataTypes.And_Bool(a,b) => fun_bool(expression)
                |   DataTypes.Or_Bool(a,b) => fun_bool(expression)
                |   DataTypes.Neg_Bool (a) => fun_bool(expression)
                |   DataTypes.Mul_Int(a,b) => fun_int(expression)
                |   DataTypes.Mul_Rat(a,b) => fun_rat(expression)
                |   DataTypes.Div_Int(a,b) => fun_int(expression)
                |   DataTypes.Div_Rat(a,b) => fun_rat(expression)
                |   DataTypes.Mod_Int(a,b) => fun_int(expression)
                |   DataTypes.Add_Int(a,b) => fun_int(expression)
                |   DataTypes.Sub_Int(a,b) => fun_int(expression)
                |   DataTypes.Add_Rat(a,b) => fun_rat(expression)
                |   DataTypes.Sub_Rat(a,b) => fun_rat(expression)
                |   DataTypes.Inverse(a) => fun_rat(expression)
                |   DataTypes.Positive(a) => helper_expression(a)
                |   DataTypes.Negative(a) => helper_expression(a)
                |   DataTypes.Equal(a,b) => fun_bool(expression)
                |   DataTypes.Not_Equal(a,b) => fun_bool(expression)
                |   DataTypes.Less(a,b) => fun_bool(expression)
                |   DataTypes.Greater(a,b) => fun_bool(expression)
                |   DataTypes.Less_Equal(a,b) => fun_bool(expression)
                |   DataTypes.Greater_Equal(a,b) => fun_bool(expression)
                |   DataTypes.Make_Rat(a,b) => fun_rat(expression)
                |   DataTypes.Rat(a) => fun_rat(expression)
                |   DataTypes.FromDecimal(a) => fun_rat(expression)
            val result = case printexp of
                DataTypes.PExp(expression) => output_data := (!output_data)^(helper_expression(expression)^"\n")
            |   DataTypes.To_Decimal(expression) => output_data := (!output_data)^(Rational.toDecimal(ex_rat_expression(expression))^"\n")
            |   DataTypes.Show_Decimal(expression) => output_data := (!output_data)^(Rational.showDecimal(ex_rat_expression(expression))^"\n")
            |   DataTypes.Show_Rat(expression) => output_data := (!output_data)^(Rational.showRat(ex_rat_expression(expression))^"\n")
        in
            DataTypes.Null
        end

    and ex_command (command : DataTypes.Command) =
        let
            val rat_var_cp = !rat_var
            val int_var_cp = !int_var
            val bool_var_cp = !bool_var
            val proc_var_cp = !proc_var
            val result =
                case command of 
                    DataTypes.Assignment_Cmd(identifier, expression) =>
                        let
                            val ident_str = getident(identifier)
                            val (rat_cond , val_dummy, sc_rat) = (value_var(rat_var_cp, ident_str))
                            val (int_cond , val_dummy, sc_int) = (value_var(int_var_cp, ident_str))
                            val (bool_cond, val_dummy, sc_bool) = (value_var(bool_var_cp, ident_str))
                            val (proc_cond, val_dummy, sc_proc) = (value_var(proc_var_cp, ident_str))
                            fun maxm(lst : (bool * int list) list) =
                                case lst of
                                    []  => 0
                                |   h::t => if (#1 h) then (if (List.length(#2 h) > maxm(t)) then List.length(#2 h) else maxm(t)) else maxm(t)
                            val max_len = maxm([(rat_cond, sc_rat), (int_cond, sc_int), (bool_cond, sc_bool), (proc_cond, sc_proc)])
                            val rat_var_cp = if (rat_cond) andalso (List.length(sc_rat) = max_len)
                                            then update(rat_var_cp, ident_str, ex_rat_expression(expression))
                                            else rat_var_cp
                            val int_var_cp = if (int_cond) andalso (List.length(sc_int) = max_len)
                                            then update(int_var_cp, ident_str, ex_int_expression(expression))
                                            else int_var_cp
                            val bool_var_cp = if (bool_cond) andalso (List.length(sc_bool) = max_len)
                                            then update(bool_var_cp, ident_str, ex_bool_expression(expression))
                                            else bool_var_cp
                            val proc_var_cp = if (proc_cond) andalso (List.length(sc_proc) = max_len)
                                            then raise_exception (ProcedureNotAssignable, "Variable "^ident_str^" is registered as a Procedure, which can not be assigned another procedure")
                                            else if (not (rat_cond orelse int_cond orelse bool_cond)) then raise_exception (VariableNotDefined, "Variable "^ident_str^" is not defined")
                                            else proc_var_cp
                            val _ = rat_var := rat_var_cp
                            val _ = int_var := int_var_cp
                            val _ = bool_var := bool_var_cp
                            val _ = proc_var := proc_var_cp
                        in
                            DataTypes.Null
                        end
                |   DataTypes.Call_Cmd(identifier) => 
                        let
                            val ident_str = getident(identifier)
                            val (found, proc_store, sc) = value_var(proc_var_cp, ident_str)
                            val (commandseq, new_scope) = if found then valOf(proc_store) else raise_exception(DatatypeNotCallable, ident_str^"is not a procedure. Only Procedures can be called")
                            val rat_var_cp =  (search_scope(new_scope, !rat_scope)) :: rat_var_cp
                            val int_var_cp =  (search_scope(new_scope, !int_scope)) :: int_var_cp
                            val bool_var_cp = (search_scope(new_scope, !bool_scope)) :: bool_var_cp
                            val proc_var_cp = (search_scope(new_scope, !proc_scope)) :: proc_var_cp
                            val _ = rat_var := rat_var_cp
                            val _ = int_var := int_var_cp
                            val _ = bool_var := bool_var_cp
                            val _ = proc_var := proc_var_cp
                            val prev_scope = !current_scope
                            val _ = current_scope := new_scope
                            val _ = ex_commandseq (commandseq)
                            val _ = current_scope := prev_scope
                            val _ = rat_var := tl rat_var_cp
                            val _ = int_var := tl int_var_cp
                            val _ = bool_var := tl bool_var_cp
                            val _ = proc_var := tl proc_var_cp
                        in
                            DataTypes.Null
                        end
                |   DataTypes.Read_Cmd(identifier) =>  
                        let
                            val ident_str = getident(identifier)
                            val (rat_cond , val_dummy, sc_rat) = (value_var(rat_var_cp, ident_str))
                            val (int_cond , val_dummy, sc_int) = (value_var(int_var_cp, ident_str))
                            val (bool_cond, val_dummy, sc_bool) = (value_var(bool_var_cp, ident_str))
                            val (proc_cond, val_dummy, sc_proc) = (value_var(proc_var_cp, ident_str))
                            fun maxm(lst : (bool * int list) list) =
                                case lst of
                                    []  => 0
                                |   h::t => if (#1 h) then (if (List.length(#2 h) > maxm(t)) then List.length(#2 h) else maxm(t)) else maxm(t)
                            val max_len = maxm([(rat_cond, sc_rat), (int_cond, sc_int), (bool_cond, sc_bool), (proc_cond, sc_proc)])
                            fun str_to_bigint (num_lst : char list, num_int : BigInt.bigint) =
                                case num_lst of
                                    []      =>  num_int
                                |   a::[]   =>  num_int
                                |   #"~"::t =>  BigInt.multiply(BigInt.int_to_bigint(~1), str_to_bigint(t, num_int))
                                |   #"+"::t => str_to_bigint(t, num_int)
                                |   a::t    => if ((ord(a)-ord(#"0")<0) orelse ((ord(a)-ord(#"0"))>9))
                                            then raise_exception(InvalidInput, "Input is not a integer value")
                                            else str_to_bigint(t,BigInt.add(BigInt.int_to_bigint(ord(a)-ord(#"0")),BigInt.multiply(BigInt.int_to_bigint(10), num_int)))
                            fun check_rat (expr : string option, state : int, index  : int) =
                                let
                                    exception UnexpectedException
                                    fun err() = raise_exception(InvalidInput, "Input is not a rational value")
                                    val exp = case expr of NONE => err() | _ => valOf(expr)
                                    val exp = String.substring(exp, 0, (size exp) - 1)
                                    val a = if ((size exp) = 0) then err() else String.sub(exp, index) 
                                    val (exp, sign) = if (a = #"~") then (String.substring(exp, 1, (size exp) - 1), 1) 
                                                        else if (a = #"+") then (String.substring(exp, 1, (size exp) - 1), 0) 
                                                        else (exp, 0)
                                    val a = if ((size exp) = 0) then err() else String.sub(exp, index) 
                                    val end_condition = (index = (size exp) -1)
                                in
                                    if state = 0 then
                                        if ((ord(a)-ord(#"0")<0) orelse ((ord(a)-ord(#"0"))>9)) then err()
                                        else if end_condition then err()
                                        else check_rat(expr, 1, index + 1)
                                    else if state = 1 then
                                        if (a = #".") then if end_condition then err() else check_rat(expr, 2, index + 1)
                                        else if ((ord(a)-ord(#"0")<0) orelse ((ord(a)-ord(#"0"))>9)) then err()
                                        else if end_condition then err() 
                                        else check_rat(expr, 1, index + 1)
                                    else if state = 2 then
                                        if (a = #"(") then if end_condition then err() else check_rat(expr, 3, index + 1)
                                        else if ((ord(a)-ord(#"0")<0) orelse ((ord(a)-ord(#"0"))>9)) then err()
                                        else if end_condition then Rational.fromDecimal(exp)
                                        else check_rat(expr, 2, index + 1)
                                    else if state = 3 then
                                        if ((ord(a)-ord(#"0")<0) orelse ((ord(a)-ord(#"0"))>9)) then err()
                                        else if end_condition then err()
                                        else check_rat(expr, 4, index + 1)
                                    else if state = 4 then
                                        if (a = #")") then if end_condition then Rational.fromDecimal(exp) else err()
                                        else if ((ord(a)-ord(#"0")<0) orelse ((ord(a)-ord(#"0"))>9)) then err()
                                        else if end_condition then err()
                                        else check_rat(expr, 4, index + 1)
                                    else raise UnexpectedException    
                                end
                            fun line() = TextIO.inputLine TextIO.stdIn;
                            val rat_var_cp = if (rat_cond) andalso (List.length(sc_rat) = max_len)
                                            then update(rat_var_cp, ident_str, check_rat(line(), 0, 0))
                                            else rat_var_cp
                            val int_var_cp = if (int_cond) andalso (List.length(sc_int) = max_len)
                                            then let
                                                    val lin = line()
                                                    fun err() = raise_exception(InvalidInput, "Input is not a integer value")
                                                in  
                                                    case lin of NONE => err()  
                                                    |   _ => update(int_var_cp, ident_str, (str_to_bigint(explode (valOf(lin)), BigInt.int_to_bigint(0))))
                                                end
                                            else int_var_cp
                            val bool_var_cp = if (bool_cond) andalso (List.length(sc_bool) = max_len)
                                            then let
                                                    val lin = line()
                                                    fun err() = raise_exception(InvalidInput, "Input is not a bool value")
                                                in  
                                                    case lin of NONE => err()  
                                                    |   _ => update(bool_var_cp, ident_str, (if valOf(lin) = "tt\n" then true else if valOf(lin) = "ff\n" then false else err()))
                                                end
                                            else bool_var_cp
                            val proc_var_cp = if (proc_cond) andalso (List.length(sc_proc) = max_len)
                                            then raise_exception (ProcedureNotAssignable, "Variable "^ident_str^" is registered as a Procedure, which can not be assigned another procedure")
                                            else if (not (rat_cond orelse int_cond orelse bool_cond)) then raise_exception (VariableNotDefined, "Variable "^ident_str^" is not defined")
                                            else proc_var_cp
                            val _ = rat_var := rat_var_cp
                            val _ = int_var := int_var_cp
                            val _ = bool_var := bool_var_cp
                            val _ = proc_var := proc_var_cp
                        in
                            DataTypes.Null
                        end
                |   DataTypes.Print_Cmd(printexp) => ex_printexp(printexp)
                |   DataTypes.Condition_Cmd(expression, commandseq1, commandseq2) =>
                        if ex_bool_expression(expression) then ex_commandseq(commandseq1) else ex_commandseq(commandseq2)
                |   DataTypes.While_Cmd(expression, commandseq) =>
                        if ex_bool_expression(expression)
                            then DataTypes.Result(ex_commandseq(commandseq), ex_command(command)) 
                        else DataTypes.Null
        in
            DataTypes.Null
        end

    and ex_commandseq (commandseq : DataTypes.CommandSeq) =
        let
            val result =
                case commandseq of
                    DataTypes.CommandSeq(commandseq2, command) => DataTypes.Result(ex_commandseq(commandseq2),ex_command(command))
                |   DataTypes.Empty => DataTypes.Null
        in
            DataTypes.Null
        end

    and ex_procedure (procedure : DataTypes.Procedure) =
        let
            val proc = !proc_scope
            val _ =
                case procedure of
                    DataTypes.Procedure(identifier, block) =>
                        if search(getident(identifier))
                            then raise_exception (MultipleDeclaration, "variable "^getident(identifier)^" declared multiple times")
                        else 
                            let
                                val (declseq, commandseq) = case block of
                                                                DataTypes.Block(decl, commands) => (decl, commands)
                                val prev_scope = !current_scope
                                val new_scope = create_scope(!current_scope, !scope_list)
                                (* val _ = HashTable.insert(#1 (hd proc))(getident(identifier), SOME (commandseq, new_scope)) *)
                                val proc = scope_insert(proc, getident(identifier), (commandseq, new_scope))
                                val _ = proc_scope := proc
                                val _ = current_scope := new_scope
                                val _ = scope_list := (new_scope :: !scope_list)
                                val rat_s = !rat_scope
                                val int_s = !int_scope
                                val bool_s = !bool_scope
                                val proc_s = !proc_scope
                                val _ = rat_scope := ((HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found")), new_scope) :: rat_s
                                val _ = int_scope := ((HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found")), new_scope) :: int_s
                                val _ = bool_scope := ((HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found")), new_scope) :: bool_s
                                val _ = proc_scope := ((HashTable.mkTable (HashString.hashString, op=) (31, Fail "not found")), new_scope) :: proc_s
                                val _ = ex_declseq(declseq)
                                val _ = current_scope := prev_scope
                            in
                                DataTypes.Null
                            end
        in
            DataTypes.Null
        end

    and ex_intvardecl (intvardecl : DataTypes.IntVarDecl) =
        let
            val result =
                case intvardecl of
                    DataTypes.IntVarDecl(identifier) => var_identifier(identifier, int_scope)
        in
            DataTypes.Null
        end
    
    and ex_intvardecl_mul (intvardecl_mul : DataTypes.IntVarDecl_Mul) =
        let
            val result =
                case intvardecl_mul of
                    DataTypes.IntVarDecl_Mul(intvardecl_mul2, intvardecl) => DataTypes.Result(ex_intvardecl_mul(intvardecl_mul2), ex_intvardecl(intvardecl))
                |   DataTypes.IntVarDecls(intvardecl) => ex_intvardecl(intvardecl)
        in
            DataTypes.Null
        end
    
    and ex_boolvardecl (boolvardecl : DataTypes.BoolVarDecl) =
        let
            val result =
                case boolvardecl of
                    DataTypes.BoolVarDecl(identifier) =>  var_identifier(identifier, bool_scope)
        in
            DataTypes.Null
        end

    and ex_boolvardecl_mul (boolvardecl_mul : DataTypes.BoolVarDecl_Mul) =
        let
            val result =
                case boolvardecl_mul of
                    DataTypes.BoolVarDecl_Mul(boolvardecl_mul1, boolvardecl) => DataTypes.Result(ex_boolvardecl_mul(boolvardecl_mul1), ex_boolvardecl(boolvardecl))
                |   DataTypes.BoolVarDecls(boolvardecl) => ex_boolvardecl(boolvardecl)
        in
            DataTypes.Null
        end

    and ex_ratvardecl (ratvardecl : DataTypes.RatVarDecl) =
        let
            val result =
                case ratvardecl of DataTypes.RatVarDecl(identifier) => var_identifier(identifier, rat_scope)
        in
            DataTypes.Null
        end

    and ex_ratvardecl_mul (ratvardecl_mul : DataTypes.RatVarDecl_Mul) =
        let
            val result =
                case ratvardecl_mul of
                    DataTypes.RatVarDecl_Mul(ratvardecl_mul, ratvardecl) => DataTypes.Result (ex_ratvardecl_mul(ratvardecl_mul), ex_ratvardecl(ratvardecl))
                |   DataTypes.RatVarDecls(ratvardecl) => ex_ratvardecl(ratvardecl)
                |   DataTypes.Phi => DataTypes.Null
        in
            DataTypes.Null
        end

    and ex_vardecls(vardecls : DataTypes.VarDecls) =
        let
            val result =
                case vardecls of
                    DataTypes.RatDecl(ratvardecl_mul) => ex_ratvardecl_mul (ratvardecl_mul)
                |   DataTypes.Declaration_Bool(vardecls, boolvardecl_mul) => DataTypes.Result(ex_vardecls(vardecls), ex_boolvardecl_mul (boolvardecl_mul))
                |   DataTypes.Declaration_intrat(ratvardecl_mul, intvardecl_mul) => DataTypes.Result (ex_ratvardecl_mul(ratvardecl_mul), ex_intvardecl_mul(intvardecl_mul))
        in
            DataTypes.Null
        end

    and ex_declseq(declseq : DataTypes.DeclSeq) =
        let
            val result =
                case declseq of
                    DataTypes.DeclSeq(declseq2, procedure) => DataTypes.Result(ex_declseq(declseq2), ex_procedure(procedure))
                |   DataTypes.DeclSeqVar(vardecls) => ex_vardecls(vardecls)
        in
            DataTypes.Null
        end

    and ex_block(block : DataTypes.Block) =
        let
            val (declseq, commandseq) =
                case block of 
                    DataTypes.Block(declseq, commandseq) => (declseq, commandseq)
            val _ = ex_declseq(declseq)
            val _ = rat_var := (search_scope([0], !rat_scope)) :: (!rat_var)
            val _ = int_var := (search_scope([0], !int_scope)) :: (!int_var)
            val _ = bool_var := (search_scope([0], !bool_scope)) :: (!bool_var)
            val _ = proc_var := (search_scope([0], !proc_scope)) :: (!proc_var)
            val _ = ex_commandseq(commandseq)
        in
            DataTypes.Null
        end

    and ex_Program(program : DataTypes.Rational_PL0) =
        let 
            val result =
                case program of 
                    DataTypes.Program(block) => ex_block(block);
        in
            DataTypes.Null
        end

    fun interpret(inputfile : string, outputfile : string) = 
        let
            val result = ex_Program(parse(inputfile))
            val output_file = TextIO.openOut outputfile
            val _ = TextIO.output(output_file, !output_data)
            val _ = TextIO.closeOut(output_file)
        in
            restore_var()
        end
end;