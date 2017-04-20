open Tile;;
open Words;;

let cFRAMESIZE = 750;;
let length = cFRAMESIZE / 18;;
let blank = new tile {id = char_of_int 32; score = 0};;

let listFind f lst = 
	match lst with
	|hd :: tl -> List.fold_left f hd tl
	|[] -> failwith "Empty";;

let compare_all tup_lst = 
	let compare lst =
		match lst with
		|[] -> true
		|hd :: tl -> List.fold_left (fun acc x -> acc && x = hd) true tl in 
	let xs, ys = List.split tup_lst in
	(compare xs, compare ys);;


let inRange x l m = 
	x >= l && x < m;;

class board = 
	object (this)

	val mutable layout = Array.make_matrix 15 15 blank
	val mutable drawPile = shuffle fullSet
	val mutable hand1 = Array.make 7 blank
	val mutable savedQ = min_int
	val mutable toggleClicked = false
	val mutable play = []

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
		
	method mouseClick mouse_x mouse_y= 
		if toggleClicked then 
			let x = mouse_x / length - 1 in
			let y = mouse_y / length - 2 in
			if (inRange x 0 15 && inRange y 0 15) then 
				(layout.(x).(y) <- hand1.(savedQ);
				hand1.(savedQ) <- blank;
				play <- (x, y):: play) else ();
			toggleClicked <- false
		else 
			let q = mouse_y / length - 6 in 
			if (inRange q 0 7 && inRange mouse_x (cFRAMESIZE - length) cFRAMESIZE) then 
			 (savedQ <- q; toggleClicked <- true)

	method is_valid () = 
		let xs, ys = List.split play in
		let xSame, ySame = compare_all play in
		if xSame then 
			(let wrd = ref [] in 
			for i = (listFind min ys) to (listFind max ys) do
				wrd := layout.(List.hd xs).(i)#getLetter :: !wrd;
			done;
			Graphics.draw_string (toString !wrd); true)
		else if ySame then true
		else false
		(*if ySame then 
			(*let x = match xs with 
				|hd::tl -> hd in
			let wrd = ref [] in 
			for i = (listFind max ys) to (listFind min ys) do
				wrd := layout.(x).(i)#getLetter :: !wrd 
			done;
			isWord !wrd*) true
		else false*)


	method refresh () = 
		play <- [];
		for i = 0 to 6 do 
			if hand1.(i)#isBlank then hand1.(i) <- this#pullTile ();
		done
(*
	method reset () = 
		let storage = ref [] in 
		List.iter (fun (x, y) -> storage := layout.(x).(y) :: !storage;
								 layout.(x).(y) <- blank) play;
		play <- [];
		for i = 0 to 6 do 
			if hand1.(i)#isBlank then 
				match !storage with
				|[] -> failwith "freakout"
				|hd :: tl -> hand1.(i) <- hd; storage := tl;
		done*)

	method reset () = 
		for i = 0 to 6 do 
			if hand1.(i)#isBlank then 
				match play with
				|[] -> failwith "freakout"
				|(x, y) :: tl -> hand1.(i) <- layout.(x).(y); play <- tl @ [(x, y)];
		done;
		List.iter (fun (x, y) -> layout.(x).(y) <- blank) play;
		
		
	
	method keyParse k = 
		if k = ' ' then 
			if this#is_valid () then this#refresh ()
			else this#reset ()
		else if k = 'r' then this#reset ()

	method react (s: Graphics.status) = 
		if s.keypressed then this#keyParse s.key
		else this#mouseClick s.mouse_x s.mouse_y
		




	end 



	
