(*
  functions to find nth fibonacci number
*)

fun fib1(n) =
  if n = 0 then 0
  else if n = 1 then 1
  else fib1(n - 1) + fib1(n - 2);

fun fib2(n) =
  if(0 <= n) andalso (n <= 1) then n
  else fib2(n - 1) + fib2(n - 2);

fun fib2'(n) =
  if (n = 0) orelse (n = 1) then n
  else fib2'(n - 1) + fib2'(n - 2);
