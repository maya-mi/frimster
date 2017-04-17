

(*We construct letters as records containing their char and score values*)
type letter = {id: char; score: int};;

let onePoint = ['E'; 'A'; 'I'; 'O'; 'N'; 'R'; 'T'; 'L'; 'S'; 'U'];;

let rec range (min : int) (max : int) : int list =
  if min > max then []
  else min :: range (min + 1) max ;;

let abc = List.map char_of_int (range 65 90);;
type word = letter list;;

