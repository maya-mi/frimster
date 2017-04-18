

(*We construct letters as records containing their char and score values*)
type letter = {id: char; score: int};;

let onePoint = ["a"; "e"; "i"; "o"; "n"; "r"; "t"; "l"; "s"; "u"] ;;
let twoPoint = ["d"; "g"] ;;
let threePoint = ["b"; "c"; "m"; "p"] ;;
let fourPoint = ["f"; "h"; "v"; "w"; "y"] ;;
let fivePoint = ["k"] ;;
let eightPoint = ["j"; "x"] ;;
let tenPoint = ["q"; "z"] ;;

let rec range (min : int) (max : int) : int list =
  if min > max then []
  else min :: range (min + 1) max ;;

let abc = List.map char_of_int (range 65 90);;
type word = letter list;;

open Hashtbl ;;

let buildTable (unit) : (string, int) Hashtbl.t  =
  let result = create 100000 in
  let reader = open_in "ospd.txt" in
  let _ = (try
    while true do
      let temp = input_line reader in
      add result temp (hash temp)
    done
  with
    End_of_file -> ()) in
  result
;;


let dict = buildTable () ;;

let detVal (input : string) : int =
  if List.mem input onePoint then 1
  else if List.mem input twoPoint then 2
  else if List.mem input threePoint then 3
  else if List.mem input fourPoint then 4
  else if List.mem input fivePoint then 5
  else if List.mem input eightPoint then 8
  else 10
;; 

(* List.fold_right, except folding over a string *)
let rec sFold (f : string -> 'a -> 'a) (acc : 'a) (s : string) : 'a =
  match s with
  | "" -> acc
  | _ -> let l = String.length s in
  	sFold f (f (String.sub s (l - 1) 1) acc) (String.sub s 0 (l - 1))
;;

let makeWord (input : string) : word =
  sFold (fun x y -> {id = String.get x 0; score = detVal x} :: y) [] input
;;
  

let getChar ({id = a; score = _} : letter) : string = String.make 1 a ;;

let getVal ({id = _; score = a} : letter) : int = a ;;

let toString (input : word) : string =
  List.fold_left (fun x y -> x ^ (getChar y)) "" input 
;;

let getScore (input : word) : int =
  List.fold_left (fun x y -> x + (getVal y)) 0 input
;;

let isWord (test : word) : bool =
  mem dict (toString test)
;;

let s = makeWord "zoo" in
print_int (getScore s) ;;

