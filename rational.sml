(* use "D:\\IIT Delhi\\Academics\\SEM 4\\COL 226\\Assignments\\Assignment 3\\rational.sml"; *)

signature BIGINT =
sig
    type bigint
    exception undefinedbigint
    val int_max: int
    val int_min: int
    val int_to_bigint: int -> bigint
    val intList_to_bigint: int list -> bigint
    val bigint_to_intList: bigint -> int list
    val compare: bigint * bigint -> int
    val add: bigint * bigint -> bigint
    val subtract: bigint * bigint -> bigint
    val multiply: bigint * bigint -> bigint
    val divide: bigint * bigint -> bigint
    val modulo: bigint * bigint -> bigint
    val set_sign: int * bigint -> bigint
    val get_sign: bigint -> int
    val show_int: bigint -> string
end;

structure BigInt :> BIGINT =
struct
    type bigint = int list  (*first ellement 0 means +; 1 means -*)
    val int_max = valOf(Int.maxInt)
    val int_min = valOf(Int.minInt)
    val int_mid = 32768
    
    exception UnexpectedException
    exception undefinedbigint

    fun print_bigint(num) =
    (*prints bigint*)
        let
            val (sign1, list1) = case num of
                                    h::t => (h,t)
                                |   _   => raise UnexpectedException
            val _ = if sign1 = 1 then print("-") else print("")
            fun p(lst : int list) =
                case lst of
                []     => let val _ = print("\n") in NONE end
            |   h::t   => let val _ = print(Int.toString(h)^" ") in p(t) end
        in
            p(list1)
        end

    fun int_to_bigint (num) =
    (*Converts int to bigint*)
        if (num >= 0) then [0,num] else [1,0-num]
    
    fun trim(lst : int list) =
        case lst of
            []      =>  [0]
        |   h::t    =>  if h=0 then trim(t) else lst

    fun moderate (sign,num_list) =
    (*Reverses and Changes int list to bigint and removes trailing zero*)
        let
            val lst = trim(List.rev(num_list))
        in
            if lst = [0] then [0,0]
            else sign::trim(List.rev(num_list))
        end

    fun intList_to_bigint (num_list) =
    (*Validates and convert intlist to bigint*)
        let
            val (sign, lst) = case num_list of
                                    h::t => (h,t)
                                |   _   => raise UnexpectedException
            fun checklist(x : int list) =
                case x of
                    []      => true
                |   h::t    => if h<0 then false else checklist(t)
        in
            if ((sign = 0) orelse (sign = 1)) andalso checklist(lst)
                then sign::trim(lst)
            else raise undefinedbigint
        end

    fun bigint_to_intList (num) = num


    fun addition (x : int list, y : int list, carry : int) =
        case (x,y) of
            ([],l)              =>  if carry=0 then l else addition([carry],l, 0)
        |   (l,[])              =>  if carry=0 then l else addition(l,[carry], 0)
        |   (h1::t1, h2::t2)    =>  if (int_max - h1) - carry < h2 
                                        then ((((h2-int_max)+h1)-1)+carry)::addition(t1,t2,1) 
                                    else  (h1+h2+carry)::addition(t1,t2,0)
                                    
    fun subtraction (x : int list, y : int list, carry : int) = (*x>y*)
        case (x,y) of
            (l,[])              =>  if carry=0 then l else subtraction(l,[0-carry],0)
        |   ([],_)              =>  raise UnexpectedException
        |   (h1::t1, h2::t2)    =>  if (h1 + carry>=h2) 
                                        then (((h1+carry)-h2)::subtraction(t1,t2,0)) 
                                    else (((int_max-h2)+carry)+h1+1)::subtraction(t1,t2,~1)

    fun mul_dig(dig1 : int, dig2 : int) =
        let
            val a1 = dig1 mod int_mid
            val a2 = dig2 mod int_mid
            val b1 = dig1 div int_mid
            val b2 = dig2 div int_mid
            val c = (a1*b2 mod int_mid)
            val d = (a2*b1 mod int_mid)
            val x = c + d
            val z = ((x + (a1*a2 div int_mid))div int_mid)
            val y = (a1*b2 div int_mid) + (a2*b1 div int_mid) + z
            val b = b1*b2 + y
            (* val a = if z = 0 then x*int_mid + a1*a2 
                    else if z =1 then (c*int_mid - (int_max-1))+(a1*a2-1)+(d*int_mid-1) 
                    else (c*int_mid - (int_max-1))+(a1*a2-3)+(d*int_mid-(int_max-1)) *)
            val a01 = (a1*a2) mod int_mid
            val carry0 = (a1*a2) div int_mid
            val a02 = (((a2*b1) mod int_mid) + ((b2*a1) mod int_mid) + (carry0 mod int_mid)) mod int_mid
            val a = int_mid * a02 + a01
        in
            (b,a)
        end
    
    fun mul(num : int list, dig : int, carry : int) =
        case num of
            []          =>  if carry = 0 then [] else [carry]
        |   h::t        =>  let
                                val (a,b) = mul_dig(h,dig)
                            in
                                if b > (int_max - carry) then ((b-int_max)+(carry-1))::mul(t, dig, a+1)
                                else (b+carry)::mul(t,dig,a)
                            end

    fun compare(num1, num2) =
    (*Compares num1 and num2.
    Returns:    1 if num1>num2
                -1 if num1<num2
                0 if num1=num2*)
        let
            val (sign1, lst1) = case num1 of
                                    h::t => (h,t)
                                |   _   => raise UnexpectedException
            val (sign2, lst2) = case num2 of
                                    h::t => (h,t)
                                |   _   => raise UnexpectedException
            val list1 = trim(lst1)
            val list2 = trim(lst2)
            fun comp(x : int list, y : int list) =
                case (x,y) of
                    ([],[])             =>  0
                |   ([], l)             =>  ~1
                |   (l, [])             =>  1
                |   (h1::t1, h2::t2)    =>  if (h1 = h2) then comp(t1, t2)
                                            else if (h1>h2) then 1
                                            else ~1
        in
            if (sign1 = sign2) then
                if List.length(list1)=List.length(list2) then
                    comp(list1, list2)
                else if List.length(list1)>List.length(list2) then
                    1
                else ~1
            else if List.length(list1) > List.length(list2) then
                if (sign1 = 0) then 1 else ~1
            else if List.length(list1) < List.length(list2) then
                if (sign1 = 0) then ~1 else 1
            else
                if (sign1 = 0) then 1 else ~1
        end

    fun int_div (num1 : int list, num2 : int list, low : int, high : int) =
        let
            val mid = (low div 2) + (high div 2) + (((low mod 2) + (high mod 2)) div 2)
            val cond1 = compare(0::List.rev(mul(List.rev(num1), mid  , 0)), 0::num2)
            val cond2 = compare(0::List.rev(mul(List.rev(num1), mid+1, 0)), 0::num2)
        in
            if cond1 = 0 then mid
            else if cond1 = 1 then int_div(num1, num2, low, mid)
            else if cond2 = 1 then mid
            else if cond2 = 0 then mid+1
            else int_div(num1, num2, mid, high)
        end

    fun division (num1 : int list, num2 : int list, rem : int list, quot : int list) =
    (*divides num2 by num1 and returns (quotient, remainder)*)
        case num2 of
            []          =>  (quot, rem)
        |   h::t        =>  let
                                val q = int_div(num1, trim(List.rev(h::rem)), 0, int_max)
                                val r = (subtraction(List.rev(trim(List.rev(h::rem))), List.rev(trim(List.rev(mul(List.rev(num1), q, 0)))), 0))
                            in
                                division (num1, t, r, q::quot)
                            end

    fun add(num1, num2) =
    (*Adds two bigint and returns their sum*)
        let
            val (sign1, lst1) = case num1 of
                                    h::t => (h,t)
                                |   _   => raise UnexpectedException
            val (sign2, lst2) = case num2 of
                                    h::t => (h,t)
                                |   _   => raise UnexpectedException
            val list1 = List.rev(lst1)
            val list2 = List.rev(lst2)
            val comp = compare(intList_to_bigint(0::lst1), intList_to_bigint(0::lst2))
            
        in
            if (sign1=sign2) 
                then  moderate(sign1, addition(list1, list2, 0))
            else if sign1 = 0 then
                if  comp > ~1
                    then moderate(0, subtraction(list1, list2, 0))
                else moderate(1, subtraction(list2, list1, 0))
            else if comp < 1
                then moderate(0, subtraction(list2, list1, 0))
            else moderate(1, subtraction(list1, list2, 0))
        end

    fun subtract(num1,num2) =
    (*Returns bigint num1 - num2*)
        let
            val (sign2, lst2) = case num2 of
                                    h::t => (h,t)
                                |   _   => raise UnexpectedException
        in
            add(num1, (1-sign2)::lst2)
        end
    
    fun multiply(num1, num2) =
    (*Multiplies and returns the product of num1 and num2*)
        let
            val (sign1, lst1) = case num1 of
                                    h::t => (h,t)
                                |   _   => raise UnexpectedException
            val (sign2, lst2) = case num2 of
                                    h::t => (h,t)
                                |   _   => raise UnexpectedException
            fun addition (x : int list, y : int list, carry : int) =
                case (x,y) of
                    ([],l)              =>  if carry=0 then l else addition([carry],l, 0)
                |   (l,[])              =>  if carry=0 then l else addition(l,[carry], 0)
                |   (h1::t1, h2::t2)    =>  if (int_max - h1) - carry < h2 
                                                then ((((h2-int_max)+h1)-1)+carry)::addition(t1,t2,1) 
                                            else  (h1+h2+carry)::addition(t1,t2,0)
            fun mul_list(num1 : int list, num2 : int list) =
                case num1 of
                    []          =>  []
                |   h::t        =>  addition(mul(num2,h,0), 0::mul_list(t, num2), 0)
            val sign = if sign1 = sign2 then 0 else 1
        in
            moderate(sign, mul_list(List.rev(lst1), List.rev(lst2)))
        end

    fun divide (num1, num2) =
    (*Divides num2 by num1 and returns quotient*)
        let 
            exception divideByZero
            val (sign1, lst1) = case num1 of
                                    h::t => (h,t)
                                |   _   => raise UnexpectedException
            val (sign2, lst2) = case num2 of
                                    h::t => (h,t)
                                |   _   => raise UnexpectedException
            val (quot, rem) = if trim(lst1)=[0] then raise divideByZero else division(lst1, lst2, [], [])
            val sign = if sign1 = sign2 then 0 else 1
        in
            moderate(sign, quot)
        end
    
    fun modulo (num1, num2) =
    (*Divides num2 by num1 and returns remainder*)
        let 
            exception divideByZero
            val (sign1, lst1) = case num1 of
                                    h::t => (h,t)
                                |   _   => raise UnexpectedException
            val (sign2, lst2) = case num2 of
                                    h::t => (h,t)
                                |   _   => raise UnexpectedException
            val (quot, rem) = if trim(lst1)=[0] then raise divideByZero else division(lst1, lst2, [], [])
        in
            moderate(sign1, rem)
        end

    fun set_sign(sign, num) = 
    (*Sets sign of num*)
        let
            val (sign1, lst1) = case num of
                                    h::t => (h,t)
                                |   _   => raise UnexpectedException
        in
            if trim(lst1) = [0] then [0,0]
            else sign::lst1
        end
    
    fun get_sign(num) =
    (*Returns the sign of num*)
        List.hd(num)

    fun show_int(num) =
    (*show_int displays a bigint in string.*)
        let
            val (sign, lst) = case num of
                                    h::t => (h,t)
                                |   _   => raise UnexpectedException
            fun str_ind(s : string,i : int) = substring(s,i,1);
            fun str_to_int(s : string) = valOf( Int.fromString (s));
            fun int_to_str(n : int) = Int.toString(n);
            (*Increments a single digit number by 1 to get a single digit number*)
            fun increment(s: string) = int_to_str(str_to_int(s)+1)
            (*Reverses a string*)
            fun reverse_str(s : string) = 
                let
                    fun reverse_str2(a : string, i : int) = 
                        if i=0 then str_ind(a,0)
                        else str_ind(a,i)^reverse_str2(a,i-1)
                in
                    if size(s) = 0 then ""
                    else reverse_str2(s,size(s)-1)
                end;
            (*Removes all initial redundant zero from string*)
            fun remove_zero(st : string) = 
                let
                    fun remove_zero2(s : string, i : int) = 
                        if (size(s)>i) andalso (str_ind(s,i)="0")
                            then remove_zero2(s,i+1)
                        else if (size(s)=i)
                            then "0"
                        else
                            substring(s,i,size(s)-i)
                in
                    remove_zero2(st,0)
                end;
            (*Adds zero to a string in starting to make it to have required digits*)
            exception ExtraSize
            fun add_zero(s : string, d : int) =
                let
                    fun zero(x : int) =
                        if x = 0 then ""
                        else if x < 0 then raise ExtraSize
                        else zero(x-1)^"0"
                in
                    zero(d-size(s))^s
                end;
            (*Returns   1 if int(a)>int(b)
                        0 if int(a)=int(b)
                        -1 otherwise        *)
            fun compare_str(s1 : string, s2 : string) =
                let
                    fun compare3(x : string, y : string, i : int) = 
                        if str_ind(x,i) > str_ind(y,i) then 1
                        else if str_ind(x,i) < str_ind(y,i) then ~1
                        else if (i+1 = size(x)) then 0
                        else compare3(x, y, i+1)

                    fun compare2(a : string, b : string) = 
                        if size(a) > size(b) then 1
                        else if size(a) < size(b) then ~1
                        else compare3(a,b,0)
                in
                    compare2(remove_zero(s1), remove_zero(s2))
                end;
            (*Returns the sum of two strings*)
            fun addition_str(s1: string, s2: string) = 
                let
                    fun add3(x : string, y : string, i : int, carry : int) =
                        let
                            val x_int = str_to_int(str_ind(x,size(x)-i))
                            val y_int = 
                                if i>size(y) then 0
                                else str_to_int(str_ind(y,size(y)-i))
                            val sum  = x_int + y_int + carry
                        in
                            if i = size(x) then reverse_str(int_to_str(sum))
                            else int_to_str(sum mod 10)^add3(x, y, i+1, sum div 10)
                        end
                    fun add2(a : string, b : string) = 
                        if compare_str(a,b) = ~1 then reverse_str(add3(b, a, 1, 0))
                        else reverse_str(add3(a, b, 1, 0))
                in
                    remove_zero(add2(remove_zero(s1),remove_zero(s2)))
                end;
            (*Multiplies an arbitrary large number s1 with a single digit s2*)
            exception NotSingleDigit;
            fun multiply_str(s1: string, s2: string) = 
                let
                    fun multiply3(x: string, y: int, i: int, carry: int) =
                        let
                            val x_int = str_to_int(str_ind(x,i))
                            val prod  = x_int * y + carry
                        in
                            if i = 0 then reverse_str(int_to_str(prod))
                            else int_to_str(prod mod 10)^multiply3(x, y, i-1, prod div 10)
                        end
                    fun multiply2(a: string, b: string) =
                        if size(b)=1 then reverse_str(multiply3(a, str_to_int(b), size(a)-1, 0))
                        else raise NotSingleDigit
                in
                    remove_zero(multiply2(remove_zero(s1),remove_zero(s2)))
                end;
            fun mul15(s : string, ct : int) =
                if ct <=0 then s else multiply_str(mul15(s, ct-1),"4")
            fun convert (num_list : int list, initial : string) =
                case num_list of
                    []      =>  initial
                |   h::t    =>  convert(t, addition_str(Int.toString(h), mul15(initial,15)))
        in
            (if sign = 1 then "~" else "")^remove_zero(convert(lst, "0"))
        end
end;

signature RATIONAL =
    sig
        type rational
        type bigint
        exception rat_error
        val make_rat: bigint * bigint -> rational option
        val rat: bigint -> rational option
        val reci: bigint -> rational option
        val neg: rational -> rational
        val inverse: rational -> rational option
        val equal: rational * rational -> bool
        val less: rational * rational -> bool 
        val add: rational * rational -> rational 
        val subtract: rational * rational -> rational 
        val multiply: rational * rational -> rational 
        val divide: rational * rational -> rational option 
        val showRat: rational -> string
        val showDecimal: rational -> string
        val fromDecimal: string -> rational
        val toDecimal: rational -> string
    end;

functor Int_to_Rational (INT : BIGINT) : RATIONAL =
    struct
        type rational = INT.bigint * INT.bigint
        type bigint = INT.bigint
        exception rat_error

        fun gcd(x: INT.bigint, y: INT.bigint) =
        (*Returns gcd of x and y*)
            let
                val zero = INT.int_to_bigint(0)
                val r = if INT.compare(y, zero) = 0 then
                            raise rat_error
                        else if INT.compare(x, zero) = 0 then
                            zero
                        else  INT.modulo(y,x)
            in
                if INT.compare(r ,zero) = 0 then y
                else gcd(y, r)
            end

        fun make_rat(x, y) =
        (*Takes any two integers and creates a rational number fractional-normal form*)
            let 
                val hcf = gcd(INT.set_sign(0,x),INT.set_sign(0,y)) 
                val (num,den) = (INT.divide(hcf,x),INT.divide(hcf,y)) 
                val sign = if INT.get_sign(x) = INT.get_sign(y) then 0 else 1
            in 
                SOME((INT.set_sign(sign, num), INT.set_sign(0, den)))
            end

        fun rat (x : INT.bigint) =
        (*inputs an integer i and converts it into the rational i/1*)
            make_rat(x, INT.int_to_bigint(1))
        
        fun reci (x : INT.bigint) =
        (*finds the reciprocal of a non-zero integer.*)
            make_rat(INT.int_to_bigint(1), x)

        fun showRat(num : rational) =
        (*showRat displays a rational number in fractional-normal form.*)
            INT.show_int(#1 num)^"/"^INT.show_int(#2 num)

        fun neg(num : rational) =
        (*takes any rational number and negates it*)
            (INT.set_sign(1-INT.get_sign(#1 num),#1 num), #2 num)
        
        fun inverse(num : rational) =
        (*finds the reciprocal of a non-zero rational.*)
            make_rat(#2 num, #1 num)
        
        fun equal(num1 : rational, num2 : rational) =
        (*equality*)
            (INT.compare(#1 num1, #1 num2) = 0) andalso (INT.compare(#2 num1, #2 num2) = 0)
        
        fun subtract(num1 : rational, num2 : rational) =
        (*subtraction: num1-num2*)
            let
                val mul1 = INT.multiply(#1 num1, #2 num2)
                val mul2 = INT.multiply(#2 num1, #1 num2)
                val num = INT.subtract(mul1,mul2)
                val den = INT.multiply(#2 num1, #2 num2)
            in
                valOf(make_rat(num,den))
            end

        fun less(num1 : rational, num2 : rational) =
        (*less than num1<num2->true*)
            let
                val diff = subtract(num1, num2)
            in
                if INT.get_sign(#1 diff) = 1 then true else false
            end
        
        fun add(num1 : rational, num2 : rational) =
        (*addition*)
            subtract(num1, neg(num2))
        
        fun multiply(num1 : rational, num2 : rational) =
        (*multiplication*)
            valOf(make_rat(INT.multiply(#1 num1, #1 num2), INT.multiply(#2 num1, #2 num2)))
        
        fun divide(num1: rational, num2: rational) =
        (*divsion num1/num2*)
            make_rat(INT.multiply(#1 num1, #2 num2), INT.multiply(#2 num1, #1 num2))
        
        fun showDecimal(num: rational) =
        (*displays the rational number in decimal-normal form*)
            let
                val I = INT.divide (#2 num, #1 num)
                val NR = INT.modulo (#2 num, #1 num)
                fun pow(num0 : INT.bigint, x : INT.bigint) =
                    let
                        val rem = INT.modulo(x,num0)
                        val q = INT.divide(x,num0)
                        val p = if INT.compare(rem,INT.int_to_bigint(0))=0 then pow(q, x) else (0,num0)
                    in
                        if INT.compare( rem, INT.int_to_bigint(0)) = 0 then
                            (1 + (#1 p), (#2 p))
                        else (0, num0)
                    end
                val two = INT.int_to_bigint(2)
                val five = INT.int_to_bigint(5)
                val ten = INT.int_to_bigint(10)
                val one = INT.int_to_bigint(1)
                val nine = INT.int_to_bigint(9)
                val zero = INT.int_to_bigint(0)
                val (p2,num2) = pow(#2 num, two)
                val (p5,den) = pow(num2, five)
                fun pow2(num, power) = 
                    if power <= 0 then  one
                    else INT.multiply(num, pow2(num, power-1))
                val p = if p2 > p5 then p2 else p5
                val numer0 = INT.multiply(NR, pow2(two, p-p2))
                val numer1 = INT.multiply(numer0, pow2(five, p-p5))
                val var = INT.show_int(INT.divide(den, numer1))
                fun zero_mul(ct : int) = if ct = 0 then "" else zero_mul(ct-1)^"0"
                val N = if p <= 0 then ""
                        else zero_mul(p-(String.size(var)))^var
                val numer = INT.modulo(den, numer1)
                fun rlen(denom:INT.bigint, n : INT.bigint, ct:int) =
                    if INT.compare(INT.modulo(denom, n),zero)=0 then (n, ct) else rlen(denom, INT.add(nine, INT.multiply(n,ten)), ct+1)
                val denominator = rlen(den, nine, 1)
                val var2 = INT.show_int(INT.multiply(INT.divide(den, #1 denominator),numer))
                val R = zero_mul(#2 denominator - String.size(var2))^var2
            in
                INT.show_int(I)^"."^N^"("^R^")"
            end

        fun fromDecimal(st0 : string) =
        (*may take a possibly non-standard decimal form and create a rational from it*)
            let
                val (sign,st) = if st0 = "" then (0,"0")
                                else if String.sub(st0,0) = #"~" then (1, String.substring(st0,1,String.size(st0)-1))
                                else if String.sub(st0,0) = #"+" then (0, String.substring(st0,1,String.size(st0)-1))
                                else (0,st0)
                val ten = INT.int_to_bigint(10)
                fun pow(num, power) = 
                    if power <= 0 then  INT.int_to_bigint(1)
                    else INT.multiply(num, pow(num, power-1))
                fun find(s:string, x:char, i:int) =
                    if String.sub(s,i) = x then i
                    else if String.size(s)-1 = i then String.size(s)
                    else find(s, x, i+1)
                val pos = find(st, #".", 0)
                val pos2 = find(st, #"(", 0)
                val pos3 = find(st, #")", 0)
                val den = pow(ten, pos2-pos-1)
                val den2 = if pos2 = pos3 then INT.int_to_bigint(1)
                            else let 
                                val den2_1 = pow(ten, pos2-pos-1)
                                val den2_2 = pow(ten, pos3-pos2-1)
                                val den2_3 = INT.subtract(den2_2, INT.int_to_bigint(1))
                                in INT.multiply(den2_1, den2_3) end
                fun com_s(s:string,x:char,i:int,last:int) =
                    if last<=i then "" else
                    (if String.sub(s,i) = x then "" else Char.toString(String.sub(s,i)))^com_s(s,x,i+1, last)
                val base = INT.intList_to_bigint([0,1,0])
                fun char_to_int(s : char) = Char.ord(s) - 48;
                fun str_to_bigint(s:string, i:int) =
                    let val x = INT.int_to_bigint(char_to_int(String.sub(s,i))) in
                        if i=0 then x else INT.add(x,INT.multiply(ten,str_to_bigint(s,i-1)))
                    end
                val num_str = if st = "." then "0" else com_s(st,#".",0,pos2)
                val num = str_to_bigint(num_str, String.size(num_str)-1)
                val num_str2 = com_s(st, String.sub(st,String.size(st)-1), pos2+1, String.size(st))
                val num2 = if pos2 = pos3 then INT.int_to_bigint(0) else str_to_bigint(num_str2, String.size(num_str2)-1)
            in
                add(valOf(make_rat(INT.set_sign(sign,num),den)), valOf(make_rat(INT.set_sign(sign,num2), den2)))
            end

        fun toDecimal(num  : rational) =
        (*converts a rational number into its standard decimal representation*)
            let
                exception NonTerminatingRational
                val I = INT.divide (#2 num, #1 num)
                val NR = INT.modulo (#2 num, #1 num)
                fun pow(num0 : INT.bigint, x : INT.bigint) =
                    let
                        val rem = INT.modulo(x,num0)
                        val q = INT.divide(x,num0)
                        val p = if INT.compare(rem,INT.int_to_bigint(0))=0 then pow(q, x) else (0,num0)
                    in
                        if INT.compare( rem, INT.int_to_bigint(0)) = 0 then
                            (1 + (#1 p), (#2 p))
                        else (0, num0)
                    end
                val two = INT.int_to_bigint(2)
                val five = INT.int_to_bigint(5)
                val ten = INT.int_to_bigint(10)
                val one = INT.int_to_bigint(1)
                val nine = INT.int_to_bigint(9)
                val zero = INT.int_to_bigint(0)
                val (p2,num2) = pow(#2 num, two)
                val (p5,den) = pow(num2, five)
                fun pow2(num, power) = 
                    if power <= 0 then  one
                    else INT.multiply(num, pow2(num, power-1))
                val p = if p2 > p5 then p2 else p5
                val numer0 = INT.multiply(NR, pow2(two, p-p2))
                val numer1 = INT.multiply(numer0, pow2(five, p-p5))
                val var = INT.show_int(INT.divide(den, numer1))
                fun zero_mul(ct : int) = if ct = 0 then "" else zero_mul(ct-1)^"0"
                val N = if p <= 0 then ""
                        else zero_mul(p-(String.size(var)))^var
                val numer = INT.modulo(den, numer1)
                fun rlen(denom:INT.bigint, n : INT.bigint, ct:int) =
                    if INT.compare(INT.modulo(denom, n),zero)=0 then (n, ct) else rlen(denom, INT.add(nine, INT.multiply(n,ten)), ct+1)
                val denominator = rlen(den, nine, 1)
                val var2 = INT.show_int(INT.multiply(INT.divide(den, #1 denominator),numer))
                val R = zero_mul(#2 denominator - String.size(var2))^var2
            in
                if R="0" then
                    INT.show_int(I)^"."^N
                else raise NonTerminatingRational
            end
    end;

structure Rational = Int_to_Rational(BigInt);
