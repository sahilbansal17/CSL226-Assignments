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
        size(normalize(s)); (* first normalize and then return the size of the string *)
end
