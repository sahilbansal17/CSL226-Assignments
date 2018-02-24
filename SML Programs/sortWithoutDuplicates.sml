(*
  A program to take a list of numbers and the binary relation R
  then sort the numbers in the list according to R and also remove the duplicates, but not removing the duplicates after sorting, rather while sorting. It is supposed to have O(nlogn) time complexity and O(n) space complexity.
 *)

(*
  using Merge Sort routine from Prof. S. Arun Kumar's programs
 *)

fun sortWithoutDuplicates R [] = []
 | sortWithoutDuplicates R [h] = [h]
 | sortWithoutDuplicates R L = (* can't split a list unless it has > 1 element *)
      let fun split []  = ([], [])
            | split [h] = ([h], [])
            | split (h1::h2::t) =
                  let val (left, right) = split t;
                   in (h1::left, h2::right)
                  end;
          val (left, right) = split L;
          fun merge (R, [], []) = []
            | merge (R, [], (L2 as h2::t2)) = L2
            | merge (R, (L1 as h1::t1), []) = L1
            | merge (R, (L1 as h1::t1), (L2 as h2::t2)) =
                     if(h1 = h2) then h1::(merge(R, t1, t2)) (* when both elements are same, keep only one of them *)
                     else if R(h1, h2) then h1::(merge (R, t1, L2))
                     else h2::(merge(R, L1, t2));
          val sortedLeft = sortWithoutDuplicates R left;
          val sortedRight = sortWithoutDuplicates R right;
      in  merge (R, sortedLeft, sortedRight)
      end;

(* Test
  val svd = sortWithoutDuplicates;
  swd (op <) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
  swd (op >) [~12, ~24, ~12, 0, 123, 45, 1, 20, 0, ~24];
*)
