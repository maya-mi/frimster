open Tile;;
open Words;;

let cFRAMESIZE = 750;;
let length = cFRAMESIZE / 17;;
class board = 
	object

	val mutable layout = Array.make_matrix 15 15 (new tile {id = char_of_int 32; score = 0})

	method draw () = 
			for i = 0 to 14 do
				for j = 0 to 14 do 
					layout.(i).(j)#draw i j;
				done
			done

	method react ({mouse_x; mouse_y; button = _ ; keypressed = _; key = _}: Graphics.status) = 
		let x = (mouse_x - length) / length in 
		let y = (mouse_y - length) / length in 
		layout.(x).(y) <- new tile {id = char_of_int 90; score = 0}

	end 



	
