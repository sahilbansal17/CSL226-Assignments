(* A program to calculate the next day given the current day in the format
    ymmdd where y can be anything > 1, mm and dd represent the two integers representing the month and day respectively *)
(* It is assumed that all the input y,m and d are positive integers *)
exception invalid_date
fun next_day(y,m,d)=
    let
      fun check_valid_day(d)=
          if d>=1 andalso d<=31 then true
          else false
      fun check_valid_month(m)=
          if m>=1 andalso m<=12 then true
          else false
      fun check_leap_year(y) =
                    if y mod 400 = 0
                    then true
                    else  if y mod 100 = 0
                    then false
                    else y mod 4 = 0
    in
      if(check_valid_day(d) <> true andalso check_valid_month(m) <> true)
      then raise invalid_date
      else
      case d of
      31 =>
          if(m = 12) then (y+1,1,1)
          else if (m = 1 orelse m = 3 orelse m = 5 orelse m = 7 orelse m = 8 orelse m = 10)
          then (y,m+1,1)
          else
          raise invalid_date
      | 30 =>
          if(m = 4 orelse m = 6 orelse m = 9 orelse m = 11)
          then (y,m+1,1)
          else if(m <> 2)
          then (y,m,d+1)
          else
          raise invalid_date
           (* this might include the case when 30 days in feb, which is not possible *)
      | 28 =>
          if(m = 2) then
            if(check_leap_year(y)) then
              (y,m,d+1)
            else
              (y,m+1,1)
          else
            (y,m,d+1)
      | 29 =>
          if(m = 2) then
            if(check_leap_year(y) <> true) then
              raise invalid_date
            else (y,m+1,1)
          else
            (y,m,d+1)
      | _ =>
          (y,m,d+1)
    end
