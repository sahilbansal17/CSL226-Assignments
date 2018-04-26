signature BIGNAT =
sig
    type bignat
    exception overflow
    exception underflow
    exception division_by_zero
    exception emptyList
    val zero : bignat
    val normalize : bignat -> bignat
    val len : bignat -> int
    val fromString : string -> bignat
    val toString : bignat -> string
    val ++ : bignat * bignat -> bignat
    val -- : bignat * bignat -> bignat
    val ** : bignat * bignat -> bignat
    (*val %% : bignat * bignat -> bignat * bignat
    val quo : bignat * bignat -> bignat
    val rem : bignat * bignat -> bignat *)
    val succ : bignat -> bignat
    val pred : bignat -> bignat
    val min : bignat * bignat -> bignat
    val max : bignat * bignat -> bignat
    val compare : bignat * bignat -> order
    val lenCompare : bignat * bignat -> order
    val << : bignat * bignat -> bool
    val <<= : bignat * bignat -> bool
    val >> : bignat * bignat -> bool
    val >>= : bignat * bignat -> bool
    val == : bignat * bignat -> bool
    val lenLt : bignat * bignat -> bool
    val lenLeq : bignat * bignat -> bool
    val lenGt : bignat * bignat -> bool
    val lenGeq : bignat * bignat -> bool
    val lenEq : bignat * bignat -> bool
    val fact : bignat -> bignat
end
