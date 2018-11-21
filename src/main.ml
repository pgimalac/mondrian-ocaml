open Geometry
open Bsp
open View
open Graphics

module Classic = View.Make (Bsp_classic)

module Extrem = View.Make (Bsp_extrem)

let rec f = ref select_mode
   
and select_mode st =
  match menu (Some st) with
  | None -> ()
  | Some Classic ->
     f := Classic.view ();
     Classic.plot ()
  | Some Extrem ->
     f := Extrem.view ();
     Extrem.plot ()

let main () =
  do_with_window
    ~on_open:(fun () -> ignore(menu None))
    (fun e -> !f e)

let _ = main ()
