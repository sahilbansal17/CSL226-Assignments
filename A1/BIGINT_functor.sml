use "BIGNAT_str.sml";
functor BigInt(Bn:BIGNAT) :
sig
    (* open Bn; *)
    type bigint;
    exception division_by_zero
    val bigzero : bigint
    val normalize : bigint -> bigint
    val bigint : int -> bigint
    val int : bigint -> int option
    (* val fromString : string -> bigint option *)
    val toString : bigint -> string
    val len : bigint -> int
    val sign : bigint -> int
    val ~~ : bigint -> bigint
    val abs : bigint -> bigint
    (* val succ : bigint -> bigint *)
    (* val pred : bigint -> bigint *)

    (* val ++ : bigint * bigint -> bigint *)
    (* val ** : bigint * bigint -> bigint *)
    (* val -- : bigint * bigint -> bigint *)
    (* val %% : bigint * bigint -> bigint * bigint *)
    (* val div : bigint * bigint -> bigint *)
    (* val mod : bigint * bigint -> bigint *)
    (* val quo : bigint * bigint -> bigint *)
    (* val rem : bigint * bigint -> bigint *)

    (* val min : bigint * bigint -> bigint *)
    (* val max : bigint * bigint -> bigint *)
    (* val sameSign : bigint * bigint -> bool *)
    (* val << : bigint * bigint -> bool *)
    (* val <<= : bigint * bigint -> bool *)
    (* val >> : bigint * bigint -> bool *)
    (* val >>= : bigint * bigint -> bool *)
    (* val == : bigint * bigint -> bool *)
    (* val compare : bigint * bigint -> order *)
    (* val lenCompare : bigint * bigint -> order *)
    (* val lenLt : bigint * bigint -> bool *)
    (* val lenLeq : bigint * bigint -> bool *)
    (* val lenGt : bigint * bigint -> bool *)
    (* val lenGeq : bigint * bigint -> bool *)
    (* val lenEq : bigint * bigint -> bool *)

end  =
struct
    (* code starts here *)
    type bigint = string; (* here we need to check whether the string starts with -, if it does then its negative *)
    exception division_by_zero
    val bigzero = Bignat.zero;
    fun sign(a) = if(String.sub(a,0) = #"-") then 1 else 0; (* 1 for negative and 0 for positive *)
    fun normalize(a) =
        let
            val last_index = size(a)-1;
            val sgn = sign(a);
        in
        if(sgn = 1) then "-"^toString(Bignat.normalize(substring(a, 1, last_index)))
        else Bignat.normalize(a)
        end
    fun bigint(a) =
        let
            val bigString = Int.toString(a);
            val firstChar = String.sub(bigString, 0);
            val last_index = size(bigString) - 1;
        in
            if(firstChar = #"~") then "-"^substring(bigString, 1, last_index)
            else bigString
        end
    (* infix ++;
    fun op ++(a, b) =
        ; *)
    fun int(a) =
        Int.fromString(a);
    fun toString(a) =
        a:string
    (* fun fromString(a) = *)
    infix ~~;
    fun op ~~(a) =
        let
            val sgn = sign(a);
            val last_index = size(a) - 1;
        in
            if(sgn = 1) then substring(a, 1, last_index)
            else "-"^a
        end
    fun abs(a) =
        let
            val sgn = sign(a);
            val last_index = size(a) - 1;
        in
            if(sgn = 1) then substring(a, 1, last_index)
            else a
        end
    fun len(a) =
        let
            val sgn = sign(a);
            val last_index = size(a) - 1;
        in
            if(sgn = 1) then last_index
            else 1+last_index
        end 
end

structure bigint = BigInt(Bignat); (* passing Bignat structure as the argument *)
open bigint;
infix ~~;
(* use "BIGINT_functor.sml"; *)
