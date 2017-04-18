open Tile;;
open Words;;

let cFRAMESIZE = 750;;
let length = cFRAMESIZE / 18;;

let inRange x l m = 
	x >= l && x < m;;

class board = 
	object (this)

	val mutable layout = Array.make_matrix 15 15 (new tile {id = char_of_int 32; score = 0})
	val mutable drawPile = shuffle fullSet
	val mutable hand1 = Array.make 7 (new tile {id = char_of_int 90; score = 0})
	val mutable savedQ = min_int
	val mutable toggleClicked = false

	method pullTile () = 
		match drawPile with
		|[] -> new tile {id = char_of_int 32; score = 0}
		|hd :: tl -> drawPile <- tl; new tile hd

	method drawBoard () = 
		for i = 0 to 14 do
			for j = 0 to 14 do 
				layout.(i).(j)#draw i j;
			done
		done

	method drawHand () = 
		for i = 0 to 6 do
			hand1.(i)#draw 16 (i + 4);
		done


	method draw () = 
		this#drawBoard ();
		this#drawHand ()

	method init () = 
		for i = 0 to 6 do 
			hand1.(i) <- this#pullTile ();
		done
		


	method react ({mouse_x; mouse_y; button = _ ; keypressed = _; key = _}: Graphics.status) = 
		if toggleClicked then 
			let x = mouse_x / length - 1 in
			let y = mouse_y / length - 2 in
			if (inRange x 0 15 && inRange y 0 15) then 
				(layout.(x).(y) <- hand1.(savedQ);
				hand1.(savedQ) <- this#pullTile ();) else ();
			toggleClicked <- false
		else 
			let q = mouse_y / length - 6 in 
			if (inRange q 0 7 && inRange mouse_x (cFRAMESIZE - length) cFRAMESIZE) then 
			 (savedQ <- q; toggleClicked <- true)



	end 



	
