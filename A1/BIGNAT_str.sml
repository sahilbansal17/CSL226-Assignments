use "BIGNAT_small_sig.sml";
structure Bignat : BIGNAT =
struct
    (* code starts here *)
    type bignat = string; (* a no can be easily represented in string form *)
    exception overflow; (* when the string length reached its limit : String.maxSize = 16777215 *)
    exception underflow; (* a natural no cannot be less than 0 *)
    exception division_by_zero;
    exception emptyList; (* length of string cannot be zero *)
    val zero = "0" (* zero *)
    fun normalize(s) =
        let
            val ls = explode(s); (* convert s into a list of chars *)
            fun normalizeCharList([#"0"]) = [#"0"]
                | normalizeCharList([]) = raise emptyList
                | normalizeCharList(h::t) =
                if(h = #"0") then normalizeCharList(t)
                else h::t; (* return the list after the first non-zero digit *)
        in
            implode(normalizeCharList(ls)) (* find the normalized Char list and then combine using implode function *)
        end
    fun len(s) =
        size(normalize(s)); (* return the size of the string *)
    fun fromString(s) =
        normalize(s) (* normalize the string to return BIGNAT *)
    fun toString(s) =
        s (* no computation required, BIGNAT is already string type *)
    (* define ++ as infix operator *)
    infix ++;
    fun op ++ (a, b) =
        let
            (* normalize the bignats *)
            val na = normalize(a);
            val nb = normalize(b);
            (* val sa = len(a);
            val sb = len(b); *)

            (* explode to list form in reverse order *)
            val la = explode(String.rev(na));
            val lb = explode(String.rev(nb));

            val res = []; (* resulting char list *)

            fun add([], [], 1, res) = #"1"::res
                | add([], [], 0, res) = res
                | add([], bh::bt, carry, res) =
                    if(ord(bh) + carry >= 58) then add([], bt, 1, chr(ord(bh) + carry - 10)::res)
                    else
                    add([], bt, 0, chr(ord(bh) + carry - 0)::res)
                | add(ah::at, [], carry, res) =
                    if(ord(ah) + carry >= 58) then add(at, [], 1, chr(ord(ah) + carry - 10)::res)
                    else
                    add([], at, 0, chr(ord(ah) + carry - 0)::res)
                | add(ah::at, bh::bt, carry, res) =
                    if(ord(ah) + ord(bh) + carry >= 106) then add(at, bt, 1, chr(ord(ah) + ord(bh) + carry - 106 + 48)::res)
                    else
                    add(at, bt, 0, chr(ord(ah) + ord(bh) + carry - 96 + 48)::res)
            val explodedRes = add(la, lb, 0, res);
        in
            implode(explodedRes)
        end
    fun lenCompare(a, b) = Int.compare(len(a), len(b));
    fun lenLt (a, b) = len(a) < len(b);
    fun lenLeq (a, b) = len(a) <= len(b);
    fun lenGt (a, b) = len(a) > len(b);
    fun lenGeq (a, b) = len(a) >= len(b);
    fun lenEq (a, b) = len(a) = len(b);

    infix << ;
    fun op << (a, b) =
        if(lenEq(a, b)) then normalize(a) < normalize(b)
        else if(lenLt(a, b)) then true
        else false

    infix <<=;
    fun op <<= (a, b) =
        if(lenEq(a,b)) then normalize(a) <= normalize(b)
        else if(lenLt(a,b)) then true
        else false

    infix >>;
    fun op >> (a, b) =
        if(lenEq(a,b)) then normalize(a) > normalize(b)
        else if(lenGt(a,b)) then true
        else false

    infix >>=;
    fun op >>= (a, b) =
        if(lenEq(a,b)) then normalize(a) >= normalize(b)
        else if(lenGt(a,b)) then true
        else false

    infix ==;
    fun op == (a, b) =
        if(lenEq(a,b)) then normalize(a) = normalize(b) (* check for equality in their normal form *)
        else false

    fun compare(a, b) =
    if(a << b) then LESS
    else if(a == b) then EQUAL
    else GREATER (* compares a with b and returns the order *)

    infix --;
    fun op -- (a,b) =
        let
            val na = normalize(a);
            val nb = normalize(b);

            val la = explode(String.rev(na));
            val lb = explode(String.rev(nb));

            val res = [];

            fun subt([], [], 0, res) = res (* no borrow possible in this case *)
                (* NOT POSSIBLE IN NATURAL NUMBER SUBTRACTION *)
                | subt([], bh::bt, borrow, res) = raise underflow
                | subt([], [], 1, res) = raise underflow
                | subt(ah::at, [], borrow, res) =
                    if(ord(ah) - borrow >= 0) then subt(at, [], 0, chr(ord(ah) - borrow)::res)
                    else
                    raise underflow
                | subt(ah::at, bh::bt, borrow, res) =
                    if(ord(ah) - ord(bh) - borrow >= 0) then subt(at, bt, 0, chr(ord(ah) - ord(bh) - borrow + 48)::res)
                    else
                    subt(at, bt, 1, chr(ord(ah) - ord(bh) - borrow + 10 + 48)::res)
            val explodedRes = subt(la, lb, 0, res);
        in
            if(a << b) then raise underflow (* result can't be negative *)
            else if(a == b) then zero
            else normalize(implode(explodedRes))
        end

        fun succ(a) = a ++ "1";
        fun pred(a) = a -- "1";
        fun min(a, b) = if(a >> b) then normalize(b) else normalize(a);
        fun max(a, b) = if(a >> b) then normalize(a) else normalize(b);
        
    infix **;
    fun op ** (a, b) = 
        let 
            val na = normalize(a);
            val nb = normalize(b);
            val la = explode(String.rev(na));
            val lb = explode(String.rev(nb));
            
            fun multiplySingleDigit(s : int, [], res, 0) = res 
            |   multiplySingleDigit(s : int, [], res, carry) = chr(carry + 48) :: res 
            |   multiplySingleDigit(s : int, la_h :: la_t, res, carry) = 
                let 
                    val laInt = ord(la_h) - 48;
                    val prod = carry + s * laInt;
                    val div10 = prod div 10;
                    val res_h = chr(prod - 10*div10 + 48);
                in
                    multiplySingleDigit(s, la_t, res_h :: res, div10)
                end
        in 
            fromString(implode(multiplySingleDigit(ord(hd(lb)) - 48, la, [], 0)))
        end
    (*
        Test: 
            val a = "123" : bignat;
            val b = "24" : bignat;
            a ** b;
            
            RESULT: The product of 4 with 123.
            
            val it = "492" : bignat 
    *)
end
open Bignat;
(* define infix operators *)
infix ++;
infix --;
infix <<;
infix <<=;
infix >>;
infix >>=;
infix ==;
infix **;
(* use "BIGNAT_str.sml"; *)
