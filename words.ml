

(*We construct letters as records containing their char and score values*)
type letter = {id: char; score: int};;

let onePoint = ['E'; 'A'; 'I'; 'O'; 'N'; 'R'; 'T'; 'L'; 'S'; 'U'];;

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
