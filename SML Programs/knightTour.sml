local

val	AllDirections =[(1, 2), (~1, 2), (1, ~2), (~1, ~2), (2, 1), (~2, 1), (2, ~1), (~2, ~1)]
		
in
	fun safe((x, y), (x0, y0)::posList, N) =
		(*  x and y should be valid  *) 
		(x >= 0) andalso (x < N) andalso (y >= 0) andalso (y < N) andalso
		((x, y) <> (x0, y0))
		andalso safe((x, y), posList, N)
		| safe _ = true

	exception Conflict;
	fun solve_knt(N, cx, cy, positions, current, directions) =
		let 
			val i2s = Int.toString
			fun pi2s (x, y) = "("^i2s(x)^", "^i2s(y)^")"
			local 
				fun lpi2s' [] = ""
					| lpi2s'(h :: t) = if (null t) then pi2s(h)
										else (pi2s h)^", "^(lpi2s' t)
			in
				fun lpi2s L = "["^(lpi2s' L)^"]"
			end
			fun showState positions = print ((lpi2s positions)^"\n")
		in
			if current = N*N
			then positions
			else case directions of
				[] => (*
						No more directions left to go
						*)
					(
						(*print("Cannot place Knight on move "^i2s(current)^":\t");*)
						(*showState(positions);*)
						raise Conflict
					)
				| (x,y)::T =>
					if(safe((cx + x, cy + y), positions, N))
						then (
							print ("Knight on move "^(i2s current)^" safe in position "^(pi2s (cx + x, cy + y))^"\n"); 
	                      	solve_knt (N, cx + x, cy + y, positions@[(cx + x, cy + y)], current+1, AllDirections)
	                      	(*
								Need to pass all the 8 directions list here
	                      	*)
			    			)
						handle Conflict =>
							if current = 1 andalso (null T)
								then ( print ("Mission Impossible\n");[])
							else(
									(*print("Backtracking to previous position:"^(pi2s(cx, cy))^"\n");*)
									solve_knt(N, cx, cy, positions, current, T)
								)
								
					else(
						(*print ("Knight on move "^(i2s current)^" unsafe in position "^(pi2s (cx + x, cy + y))^"\n"); *)
						(*print ("Baktracking to other directions possible at:"^(pi2s(cx, cy))^"\n");*)
						solve_knt(N, cx, cy, positions, current, T)
						)
						(*
							for the case when n = 2
							
							handle Conflict =>
							if(n = 2) then print ("Mission Impossible\n")

							raised impossible exception to make it simple
						*)
						
		end;

	exception Impossible;
	fun knt(N) =
			if N <= 2 then raise Impossible
			else solve_knt(N, 0, 0, [(0,0)], 1, AllDirections)
end;