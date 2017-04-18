open Words ;;
open Tile ;;


let getWords (board : tile array array) (tiles : tile array) (timer : float) : (tile * int * int) list = 
  let bestWord = [] in
  let _ = while timer > 0. do
    ignore (timer = timer -. 0.05) ;
  done in
  []

;;

let rec interleave_help (x: int) (l: int list) (r: int list) (combos: int list list): (int list) list = 
  match r with
  |[] -> (l @ [x]) :: combos
  |hd :: tl -> interleave_help x (l @ [hd]) tl (((l @ [x]) @ r) :: combos);;

(*Passes the interwoven x and list of interest to interleave_help, along
with the initially empty list representing the values to the left of x*)
let interleave (x: int) (lst: int list): (int list) list = 
  interleave_help x [] lst [];;

(*Recursively interleaves the current first element of the int list with
the permutations of all previous elements, using List.map and List.concat*)
let rec perm_help (ints: int list) (combos: int list list) = 
  match ints with
  |[] -> combos
  |hd:: tl -> perm_help tl (List.concat (List.map (fun lst -> interleave hd lst) combos))
;;

(*Passes the int list and a list of combos of [], or [[]], to perm_help*)
let permutations (ints: int list) : (int list) list =
	perm_help ints [[]]
;;

let permTiles (tiles : tile array) : unit =
	()
;;

let bestWordH (board : tile array array) (tiles : tile array) ((x,y) : int * int) : (tile * int * int) list =
	[]
;;

exception OffBoard ;;

let scoreTiles (move : (tile * int * int) list) : int =
  try 
    let toWord (letters : (tile * int *int) list) : word =
      List.fold_right (fun n acc -> 
        match n with
        | t, x, y -> if min x y < 0 || max x y >= 15 then raise OffBoard else
          t#getLetter :: acc) letters []
    in
    let attempt = toWord move in
    if not (isWord attempt) then -1 else getScore attempt 
  with OffBoard -> -1
;;