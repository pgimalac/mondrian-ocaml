open Bsp
open View
open Graphics

let main () =
  let bsp =
    L (
        {pt1 = {x=200.; y=0.}; pt2 = {x=200.;y=600.}; c = Red},
        R (None),
        R (None))
  in
  let bsp = insert bsp {pt1 = {x=0.;y=0.}; pt2 = {x=500.;y=500.}; c = Blue} in
  View.do_with_window (fun () -> View.plot_bsp bsp)

let _ = main ()
