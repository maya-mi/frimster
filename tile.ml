open Word;;

(*class virtual tile ?(c: letter = {id = char_of_int 32; score = 0}) 
					(x: int) (y: int) = 
 object
	val ch: char 
	val score: int
	val x : int
	val y: int

	method draw : unit -> unit
	method getX : int
	method getY : int

	method init: letter -> unit
 end	*)


class tile ?(c: letter = {id = char_of_int 32; score = 0}) 
			(x: int) (y: int) = 
 object (this)
 	val mutable ch = c.id
 	val mutable score = id.score
 	val x = x
 	val y = y

 	method getX = this#x
 	method getY = this#y

 	method init {id; score = n} : unit = 
 		ch := id;
 		score := n

 	method draw () = 
 		Graphics.draw_rect x y 100 100
 end



