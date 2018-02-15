(*
  check whether a given year is leap or not
*)

fun leap y =  if y mod 400 = 0
              then true
              else  if y mod 100 = 0
                    then false
                    else y mod 4 = 0
(*
  used the fact that
  CONDITION is equivalent to:
  if CONDITION then true else false
*)

(*
  another way of writing the same function in terms of structural simplicity
*)

fun leap2 y =
    let
        fun century x = (x mod 100 = 0);
    in
        if(century y)
        then y mod 400 = 0
        else y mod 4 = 0
    end
