use "BIGNAT_str.sml";
functor BigInt(Bn:BIGNAT) :
sig
    (* open Bn; *)
    type bigint;
    exception division_by_zero
    val bigzero : bigint
    val normalize : bigint -> bigint
    (* val normalize : bigint -> bigint
    val bigint : int -> bigint
    val int : bigint -> int option
    val fromString : string -> bigint option
    val toString : bigint -> string
    val len : bigint -> int
    val sign : bigint -> int
    val ~~ : bigint -> bigint
    val abs : bigint -> bigint
    val succ : bigint -> bigint
    val pred : bigint -> bigint

    val ++ : bigint * bigint -> bigint
    val ** : bigint * bigint -> bigint
    val -- : bigint * bigint -> bigint
    val %% : bigint * bigint -> bigint * bigint
    val div : bigint * bigint -> bigint
    val mod : bigint * bigint -> bigint
    val quo : bigint * bigint -> bigint
    val rem : bigint * bigint -> bigint

    val min : bigint * bigint -> bigint
    val max : bigint * bigint -> bigint
    val sameSign : bigint * bigint -> bool
    val << : bigint * bigint -> bool
    val <<= : bigint * bigint -> bool
    val >> : bigint * bigint -> bool
    val >>= : bigint * bigint -> bool
    val == : bigint * bigint -> bool
    val compare : bigint * bigint -> order
    val lenCompare : bigint * bigint -> order
    val lenLt : bigint * bigint -> bool
    val lenLeq : bigint * bigint -> bool
    val lenGt : bigint * bigint -> bool
    val lenGeq : bigint * bigint -> bool
    val lenEq : bigint * bigint -> bool
    *)
end  =
struct
    (* code starts here *)
    type bigint = string; (* here we need to check whether the string starts with -, if it does then its negative *)
    exception division_by_zero
    val bigzero = Bignat.zero;
    fun normalize(a) = Bignat.normalize(a);
end

structure bigint = BigInt(Bignat); (* passing Bignat structure as the argument *)
open bigint;
(* use "BIGINT_functor.sml"; *)
