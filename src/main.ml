open View

let rec handler = ref select_mode

and select_mode st =
  match menu (Some st) with
  | None -> ()
  | Some Classic ->
     let module B = View.Make (Classic.Bsp) in
     handler := B.view ();
     B.plot ()
  | Some Extrem ->
     let module B = View.Make (Extrem.Bsp) in
     handler := B.view ();
     B.plot ()

let main () =
  do_with_window
    ~on_open:(fun () -> ignore(menu None))
    (fun e -> !handler e)

let _ = main ()
