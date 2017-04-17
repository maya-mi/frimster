open Tile;;
open Word;;

class Board = 
	object

	
		method draw () = 
			for i = 0 to 14 do
				for j = 0 to 14 do 
					Graphics.draw_rect (i * 15) (j * 15) 15 15;
				done
			done



	
