open Tile;;
open Word;;

class Board = 
	object
		method draw () = 
			let rec loop i = 
				for j = 0 to 14
					if j < 15 then 
						Graphics.draw_rect (i * 15) (j * 15) 15 15;
						loop (i + 1)



	
