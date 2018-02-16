(*
  Computing a_n*x^n + a_(n-1)*x^(n-1) + ... + a_1*x^1 + a_0
  where the first argument of the function poly0 is
  the list [a_n, a_(n-1), ..., a1, a0]
  and the second argument is
  the value of x at which the polynomial is to be evaluated
*)

(*
  if the list is empty then answer is 0
  base case for the recursion

  since total elements in the list is n+1, the first element is multiplied by x^n i.e. x^(length of list ahead of a_n)

  so call is made to poly0(remaining list, x)
*)

fun poly0 ([], x) = 0.0
    | poly0 ((h :: T), x) =
      let
        val n = Real.fromInt (List.length T) (* Need to convert length from Int to Real since power function used in next line takes Real arguments *)
      in
        h * Math.pow(x, n) + poly0(T, x)
      end
