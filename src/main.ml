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
  if Array.length Sys.argv >= 3
  then begin
    let x = int_of_string_opt Sys.argv.(1) in
    let y = int_of_string_opt Sys.argv.(2) in
    match x, y with
    | Some x, Some y ->
      if x >= 600 && y >=600
      then begin
        set_window_width x; set_window_height y
      end
    | _, _ -> ()
  end;
  do_with_window
    ~on_open:(fun () -> ignore(menu None))
    (fun e -> !handler e)

let _ = main ()
