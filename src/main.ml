open Bsp
open View
open Graphics
   
let main () =
  let bsp = ref
    (L (
        {pt1 = {x=200.; y=0.}; pt2 = {x=200.;y=600.}; c = Some Red},
        R (None),
        R (None)))
  in
  bsp :=
    insert !bsp {pt1 = {x=200.;y=500.}; pt2 = {x=600.;y=550.}; c = Some Blue};
  bsp :=
    insert !bsp {pt1 = {x=0.;y=500.}; pt2 = {x=200.;y=200.}; c = None};
  bsp :=
    insert !bsp {pt1 = {x=600.;y=100.}; pt2 = {x=200.;y=200.}; c = None};
  View.do_with_window
    ~on_open:(fun () -> View.plot_bsp !bsp)
    (fun e ->
      if e.button
      then bsp := change_color !bsp {
               x=float_of_int e.mouse_x;
               y=float_of_int e.mouse_y};
      
      View.plot_bsp !bsp)

let _ = main ()
