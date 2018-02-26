(*
  This program is used to solve the standard Tower of Hanoi puzzle.
*)

(*
  Generate a list containing the disks from 1 to n, which is the rod from where we will transfer
*)
fun genList(L, i, N)
  if(i == N) then L
  else genList(i::L, i+1, N)
