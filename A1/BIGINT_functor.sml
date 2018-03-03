functor BigInt(Bn:BIGNAT) :
sig
    type bigint
    exception division_by_zero
    val bigzero : bigint
    val normalize : bigint -> bigint
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
    val div : bigint * bigint -> bigint
    val mod : bigint * bigint -> bigint
    val quo : bigint * bigint -> bigint
    val rem : bigint * bigint -> bigint
end  =
struct
    (* code starts here *)
end
