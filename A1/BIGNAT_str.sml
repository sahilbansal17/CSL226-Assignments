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
        size(s); (* return the size of the string *)
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
                    add(bt, at, 0, chr(ord(ah) + ord(bh) + carry - 96 + 48)::res)
            val explodedRes = add(la, lb, 0, res);
        in
            implode(explodedRes)
        end
end
(* define infix operators *)
infix ++;
