(*
            Solving and rendering force-directed graphics
                          CS51 Problem Set 7
                         -.-. ... ..... .----
 *)
open Graphics ;;
open Printf ;;
open Board;;
open Tile;;

(*......................................................................
  Configuration 
 *)
  
(* Minimum time between displaying successive frames *)
let cFRAMEDELAY = 1. /. 1000. ;;
(* Macimum number of frames (time steps) before giving up on reaching
   quiescence *)
let cMAXFRAMES = 2000 ;;

let cSTEPPING = ref false ;;
let windowSize = 750;;




(*......................................................................
  A solver that animates the solution process using OCaml's X11
  graphics support
 *)

module G = Graphics ;;

let rec delay (sec: float) : unit =
  try ignore(Thread.delay sec)
  with Unix.Unix_error _ -> delay sec

let framedelay () = delay cFRAMEDELAY ;;

let x11_initialize_update iters_left =
  if !cSTEPPING then
    (let char = G.read_key () in
     if char = 'x' then
       (printf "User requested exit\n";
        flush stdout;
        raise Exit)) ;;

let x11_initialize () =
  (* open a graphics window to draw into and size it appropriately *)
  G.open_graph "";
  G.resize_window windowSize windowSize;
  (* turn off auto synchronizing; we'll handle double buffer
     synchronization ourselves *)
  G.auto_synchronize false;
  G.display_mode false;;

let x11_finalize () =
  (* Close the window on keystroke *)
  ignore (G.read_key ()) ;;

let listen () = 
	x11_initialize ();
	let b = new board in 
	b#draw ();
	while true do 
		let s = Graphics.wait_next_event [Graphics.Button_down] in 
		b#react s;
		b#draw ();
		x11_finalize ();
	done;;
	
listen ();;




(*open Printf;;
open Board;;
let cFRAMESIZE = 500;;

module G = Graphics;;

let initialize () =
  (* open a graphics window to draw into and size it appropriately *)
  G.open_graph "";
  G.resize_window cFRAMESIZE cFRAMESIZE;
  (* turn off auto synchronizing; we'll handle double buffer
     synchronization ourselves *)
  G.auto_synchronize false;
  G.display_mode false;;

let finalize () =
  (* Close the window on keystroke *)
  ignore (G.read_key ()) ;;*)