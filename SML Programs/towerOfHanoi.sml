(*
  This program is used to solve the standard Tower of Hanoi puzzle.
*)

(*
  Generate a list containing the disks from 1 to n, which is the rod from where we will transfer
*)
(* fun genList(L, i, N) =
  if i=N+1 then L
  else genList(i::L, i+1, N) *)

fun solveToH(n,from,to,aux) =
    if n = 1 then
    print("\nMove disk 1 from rod "^Int.toString(from)^" to rod "^Int.toString(to))
    else(
      solveToH(n-1,from,aux,to);
      print("\nMove disk "^Int.toString(n)^" from rod "^Int.toString(from)^" to rod "^Int.toString(to));
      solveToH(n-1,aux,to,from)
      )

(* Test:
solveToH(3,1,3,2); // move 3 disks from tower 1 to tower 3 using 2 as the auxiliary tower
 *)
