open Graphics;;
open Board;;

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
  ignore (G.read_key ()) ;;