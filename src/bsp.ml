open Graphics
open Geometry
open Sat_solver

module type Bsp_type = sig

  type bsp

  val change_color : ?reverse:bool -> bsp -> point -> bsp

  val generate_random_bsp : float -> float -> bsp

  val color_nth : bsp -> int -> color -> bsp

  val iter_area : (int -> color -> point list -> unit) -> bsp -> float -> float -> unit

  val iter_line : (int -> line -> color -> unit) -> bsp -> float -> float -> unit

  val iter_line_area : (int -> line -> color -> unit) -> (int -> color -> point list -> unit) -> bsp -> float -> float -> unit

  val get_lines_area : bsp -> int -> (color * ((color * int) list)) array

  val get_number_lines : bsp -> int

  val get_number_areas : bsp -> int

  val clean : bsp -> bsp
end

let _ = Random.self_init ()

let random_color () =
  if Random.float 2. < 1.
  then blue
  else red

let colors = [white; red; blue]
let next_color reverse c =
  let rec aux tab =
    match tab with
    | hd1 :: hd2 :: tl when hd1 = c -> hd2
    | hd :: [] when hd = c -> List.hd colors
    | hd :: tl -> aux tl
    | [] -> aux colors
  in
  let rec aux_r tab =
    match tab with
    | hd1 :: hd2 :: tl when hd2 = c -> hd1
    | hd :: tl -> aux_r tl
    | [] -> List.nth colors ((List.length colors) - 1)
  in (if reverse then aux_r else aux) colors


module Bsp_extrem : Bsp_type = struct
  let min_area = 2000.

  type bsp = R of int * color | L of int * line * color * bsp * bsp

  let iter_area f bsp bound_x bound_y =
    let rec find_polygone bsp pts =
      match bsp with
      | L (_, l, _, left, right) ->
         let left_pts, right_pts = split_by_line l pts in
         find_polygone left left_pts;
         find_polygone right right_pts;
         ()
      | R (n, c) ->
         let barycenter = center pts in
         let pts = List.sort (compare_counter_clockwise barycenter) pts in
         f n c pts
    in
    let pts, lines = edges bound_x bound_y in
    find_polygone bsp pts

  let rec iter_line f bsp bound_x bound_y =
    match bsp with
    | L (n, l, c, left, right) ->
       f n l c;
       iter_line f left bound_x bound_y;
       iter_line f right bound_x bound_y
    | R _ -> ()

  let iter_line_area f_line f_area bsp bound_x bound_y =
    let rec find_polygone bsp pts =
      match bsp with
      | L (n, l, c, left, right) ->
         f_line n l c;
         let left_pts, right_pts = split_by_line l pts in
         find_polygone left left_pts;
         find_polygone right right_pts;
         ()
      | R (n, c) ->
         let barycenter = center pts in
         let pts = List.sort (compare_counter_clockwise barycenter) pts in
         f_area n c pts
    in let pts, lines = edges bound_x bound_y in
    find_polygone bsp pts

  let rec add_random_line localBsp pts localDepth =
    if localDepth = 0
    then localBsp
    else
      match localBsp with
      | L (_, l, c, left, right) ->
         let leftPts, rightPts =
           separate_points l ([l.pt1; l.pt2], [l.pt1; l.pt2]) pts in
         if Random.float 1. < 0.5
         then L(0, l, c, add_random_line left leftPts (localDepth - 1), right)
         else L(0, l, c, left, add_random_line right rightPts (localDepth - 1))
      | R (_, c) ->
         let pts = List.sort (compare_counter_clockwise (center pts)) pts in
         let ptsArr = Array.of_list pts in
         let new_line = gen_random_lines ptsArr in
         let left, right =
           separate_points new_line
             ([new_line.pt1; new_line.pt2], [new_line.pt1; new_line.pt2]) pts
             (* we have to do that because the center changes *) in
         let left = List.sort (compare_counter_clockwise (center left)) left in
         let right = List.sort (compare_counter_clockwise (center right)) right in
         if polygon_area left > min_area && polygon_area right > min_area
         then L(0, new_line, black, R (0, random_color ()), R (0, random_color ()))
         else localBsp

  and gen_random_bsp width height nb_lines maxDepth =
    if maxDepth >= 0 && (2. ** (float_of_int maxDepth)) >= (float_of_int nb_lines /. 2.)
    then R (0, random_color ())
    else
      let bsp = ref (R (0, random_color ())) in
      let v, _ = edges width height in
      for _ = 1 to nb_lines do
        bsp := add_random_line !bsp v maxDepth
      done;
      !bsp

  let rec change_color ?(reverse=false) bsp pt =
    match bsp with
    | L (n, l, c, left, right) ->
       let left, right =
         if is_left pt l
         then change_color ~reverse:reverse left pt, right
         else left, change_color ~reverse:reverse right pt
       in
       L (n, l, c, left, right)
    | R (n, c) -> R (n, next_color reverse c)

  let rec color_nth bsp n color =
    match bsp with
    | L (i, l, c, left, right) ->
      L (i, l, c, color_nth left n color, color_nth right n color)
    | R (i, origin) ->
      if n = i then R (i, color)
      else R (i, origin)

  let rec add_random_line localBsp pts localDepth =
    if localDepth = 0
    then localBsp
    else
      match localBsp with
      | L (_, l, c, left, right) ->
         let leftPts, rightPts = separate_points l ([l.pt1; l.pt2], [l.pt1; l.pt2]) pts
         in if Random.float 1. < 0.5
            then L(0, l, c, add_random_line left leftPts (localDepth - 1), right)
            else L(0, l, c, left, add_random_line right rightPts (localDepth - 1))
      | R (_, c) ->
         let pts = List.sort (compare_counter_clockwise (center pts)) pts in
         let ptsArr = Array.of_list pts in
         let new_line = gen_random_lines ptsArr in
         let left, right =
           separate_points new_line
             ([new_line.pt1; new_line.pt2], [new_line.pt1; new_line.pt2]) pts
         (* we have to do that because the center changes *) in
         let left = List.sort (compare_counter_clockwise (center left)) left in
         let right = List.sort (compare_counter_clockwise (center right)) right in
         if polygon_area left > min_area && polygon_area right > min_area
         then L(0, new_line, black, R (0, random_color ()), R (0, random_color ()))
         else localBsp

  and gen_random_bsp width height nb_lines maxDepth =
    if maxDepth >= 0 && (2. ** (float_of_int maxDepth)) >= (float_of_int nb_lines /. 2.)
    then R (0, random_color ())
    else
      let bsp = ref (R (0, random_color ())) in
      let v, _ = edges width height in
      for _ = 1 to nb_lines do
        bsp := add_random_line !bsp v maxDepth
      done;
      !bsp

  let get_number_lines bsp =
    let rec getNbLines bsp i =
      match bsp with
      | L (n, _, _, _, r) -> getNbLines r n
      | R _ -> i + 1
    in getNbLines bsp 0

  let rec get_number_areas bsp =
    match bsp with
    | L (_, _, _, _, r) -> get_number_areas r
    | R (n, _) -> n + 1

  let get_lines_area bsp number_lines =
    let rec separate l (left, right) (i, c, pt1, pt2) =
      if is_left pt1 l && is_left pt2 l then (i, c, pt1, pt2) :: left, right
      else if is_right pt1 l && is_right pt2 l then left, (i, c, pt1, pt2) :: right
      else
        let inter =
        if is_on_line {pt1 = pt1; pt2 = pt2} l.pt1
        then l.pt1
        else l.pt2
        in let ptL, ptR =
          if is_left pt1 l
          then pt1, pt2
          else pt2, pt1
        in (i, c, ptL, inter) :: left, (i, c, ptR, inter) :: right
    in let arr = Array.make number_lines (black, []) in
    let rec fillBsp bsp pts =
      match bsp with
      | L (n, l, c, left, right) ->
        let lPts, rPts = List.fold_left (separate l) ([n, c, l.pt1, l.pt2], [n, c, l.pt1, l.pt2]) pts in
        fillBsp left lPts;
        fillBsp right rPts
      | R (n, color) ->
          List.iter (fun (i, c, _, _) ->
            let _, li = arr.(i) in
            arr.(i) <- c, (color, n) :: li) pts
    in fillBsp bsp []; arr

  let rec clean bsp =
    match bsp with
    | L (n, l, c, left, right) ->
      L (n, l, c, clean left, clean right)
    | R (n, _) -> R (n, white)

  let generate_random_bsp width height =
    let rec index bsp nbL nbA =
      match bsp with
      | L (_, l, c, left, right) ->
        let lBsp, nbLBspLeft,  nbABspLeft  = index left  nbL        nbA in
        let rBsp, nbLBspRight, nbABspRight = index right (nbLBspLeft + 1) nbABspLeft in
        L (nbLBspLeft, l, c, lBsp, rBsp), nbLBspRight, nbABspRight
      | R (_, c) -> R (nbA, c), nbL, (nbA + 1)
    in let bsp, nbL, _ = index (gen_random_bsp width height 100 (-1)) 0 0 in
    let arr = get_lines_area bsp nbL in
    let rec color_bsp bsp =
      match bsp with
      | L (n, l, _, left, right) ->
        let _, li = arr.(n) in
        let acc = List.fold_left (fun acc (c, _) -> acc + (if c = red then 1 else -1)) 0 li in
        let c = if acc = 0 then green else if acc > 0 then red else blue in
        L (n, l, c, color_bsp left, color_bsp right)
      | a -> a
    in clean (color_bsp bsp)

end

module Bsp_classic : Bsp_type = struct
  type bsp = L of int * float * color * bsp * bsp | R of int * color

  exception ToSmallArea
  let min_area = 50.
  let area_range = min_area /. 5.

  let iter_area f bsp bound_x bound_y =
    let rec iter_area_depth bsp pt1 pt2 depth =
      match bsp with
      | L (_, v, _, left, right) ->
         if depth mod 2 = 0
         then begin
             iter_area_depth left pt1 {x = v; y = pt2.y} (depth + 1);
             iter_area_depth right {x = v; y = pt1.y} pt2 (depth + 1)
           end
         else begin
             iter_area_depth left pt1 {x = pt2.x; y = v} (depth + 1);
             iter_area_depth right {x = pt1.x; y = v} pt2 (depth + 1)
           end
      | R (n, color) ->
         f n color [pt1; {x = pt2.x; y = pt1.y}; pt2; {x = pt1.x; y = pt2.y}]
    in
    iter_area_depth bsp {x = 0.; y = 0.} {x = bound_x; y = bound_y} 0

  let iter_line f bsp bound_x bound_y =
    let rec iter_line_depth bsp pt1 pt2 depth =
      match bsp with
      | L (n, v, c, left, right) ->
         if depth mod 2 = 0
         then begin
             f n {pt1 = {x = v; y = pt1.y}; pt2 = {x = v; y= pt2.y}} c;
             iter_line_depth left pt1 {x = v; y = pt2.y} (depth + 1);
             iter_line_depth right {x = v; y = pt1.y} pt2 (depth + 1)
           end
         else begin
             f n {pt1 = {x = pt1.x; y = v}; pt2 = {x = pt2.x; y = v}} c;
             iter_line_depth left pt1 {x = pt2.x; y = v} (depth + 1);
             iter_line_depth right {x = pt1.x; y = v} pt2 (depth + 1)
           end
      | R _ -> ()
    in
    iter_line_depth bsp {x = 0.; y = 0.} {x = bound_x; y = bound_y} 0

  let iter_line_area f_line f_area bsp bound_x bound_y =
    let rec iter bsp pt1 pt2 depth =
      match bsp with
      | L (n, v, c, left, right) ->
         if depth mod 2 = 0
         then begin
             f_line n {pt1 = {x = v; y = pt1.y}; pt2 = {x = v; y= pt2.y}} c;
             iter left pt1 {x = v; y = pt2.y} (depth + 1) ;
             iter right {x = v; y = pt1.y} pt2 (depth + 1)
           end
         else begin
             f_line n {pt1 = {x = pt1.x; y = v}; pt2 = {x = pt2.x; y = v}} c;
             iter left pt1 {x = pt2.x; y = v} (depth + 1);
             iter right {x = pt1.x; y = v} pt2 (depth + 1)
           end
      | R (n, color) ->
         f_area n color [pt1; {x = pt2.x; y = pt1.y}; pt2; {x = pt1.x; y = pt2.y}]
    in
    iter bsp {x = 0.; y = 0.} {x = bound_x; y = bound_y} 0

  let change_color ?(reverse=false) bsp pt =
    let rec change_color_depth bsp pt depth =
      match bsp with
      | L (n, v, c, left, right) ->
         let is_left =
           if depth mod 2 = 0
           then pt.x < v
           else pt.y < v in
         if is_left
         then L(n, v, c, change_color_depth left pt (depth + 1), right)
         else L(n, v, c, left, change_color_depth right pt (depth + 1))
      | R (n, r) -> R (n, next_color reverse r)
    in
    change_color_depth bsp pt 0

  let rec color_nth bsp n color =
    match bsp with
    | L (i, l, c, left, right) ->
      L (i, l, c, color_nth left n color, color_nth right n color)
    | R (i, origin) ->
      if n = i then R (i, color)
      else R (i, origin)

  let get_lines_area bsp number_lines =
    let arr = Array.make number_lines (black, []) in
    let rec fillBsp bsp (n, w, s, e) depth =
      match bsp with
      | L (i, _, c, left, right) ->
        if depth mod 2 = 0
        then begin
            fillBsp left  (n, w, s, Some (c,i)) (depth + 1);
            fillBsp right (n, Some (c,i), s, e) (depth + 1)
          end
        else begin
            fillBsp left  (Some (c,i), w, s, e) (depth + 1);
            fillBsp right (n, w, Some (c,i), e) (depth + 1)
          end
      | R (i, c) ->
        let add n =
          match n with
          | None -> ()
          | Some (color, f) ->
            let _, l = arr.(f) in
            arr.(f) <- (color, (c, i) :: l)
        in add n; add w; add s; add e
    in fillBsp bsp (None, None, None, None) 0; arr

  let rec clean bsp =
    match bsp with
    | L (n, f, c, left, right) ->
       L (n, f, c, clean left, clean right)
    | R (n , _) -> R (n, white)

  let generate_random_bsp bound_x bound_y =
    let rec index bsp nbL nbA = (* indexes lines and areas *)
      match bsp with
      | L (_, l, c, left, right) ->
        let lBsp, nbLBspLeft,  nbABspLeft  = index left  nbL        nbA in
        let rBsp, nbLBspRight, nbABspRight = index right (nbLBspLeft + 1) nbABspLeft in
        L (nbLBspLeft, l, c, lBsp, rBsp), nbLBspRight, nbABspRight
      | R (_, c) -> R (nbA, c), nbL, (nbA + 1)
    and gen_while max min =
      let i = (Random.float (max -. min)) +. min in
      if max -. i < area_range || i -. min < area_range
      then gen_while max min
      else i
    and gen_rand max min =
      if max -. min < min_area
      then raise ToSmallArea
      else gen_while max min
    and generate_random_bsp_depth pt1 pt2 depth =
      try
        if depth mod 2 = 0
        then let x = gen_rand pt2.x pt1.x in
             L (0, x, black,
                generate_random_bsp_depth pt1 {x = x; y = pt2.y} (depth + 1),
                generate_random_bsp_depth {x = x; y = pt1.y} pt2 (depth + 1))
        else let y = gen_rand pt2.y pt1.y in
             L (0, y, black,
                generate_random_bsp_depth pt1 {x = pt2.x; y = y} (depth + 1),
                generate_random_bsp_depth {x = pt1.x; y = y} pt2 (depth + 1))
      with _ -> R (0, random_color ())
    in let bsp = generate_random_bsp_depth {x = 0.; y = 0.} {x = bound_x; y = bound_y} 0 in
    let bsp, nbL, _ = index bsp 0 0 in
    let arr = get_lines_area bsp nbL in
    let rec color_bsp bsp =
      match bsp with
      | L (n, l, _, left, right) ->
        let _, li = arr.(n) in
        let acc = List.fold_left (fun acc (c, _) -> acc + (if c = red then 1 else -1)) 0 li in
        let c = if acc = 0 then green else if acc > 0 then red else blue in
        L (n, l, c, color_bsp left, color_bsp right)
      | a -> a
    in clean (color_bsp bsp)

  let get_number_lines bsp =
    let rec getNbLines bsp i =
      match bsp with
      | L (n, _, _, _, r) -> getNbLines r n
      | R _ -> i + 1
    in getNbLines bsp 0

  let rec get_number_areas bsp =
    match bsp with
    | L (_, _, _, _, r) -> get_number_areas r
    | R (n, _) -> n + 1

  let get_fnc bsp =
    let nb_lines = get_number_lines bsp in
    let arr = get_lines_area bsp nb_lines in
    let print_fnc f = (* for debug purpose *)
      List.iter (fun x ->
        print_string "[";
        List.iter (fun (b, n) -> if not b then print_string "not "; print_int n; print_string " ") x;
        print_string "] et ";
      ) f;
      print_newline () in
    let print_aja i = (* for debug purpose *)
      let c, l = arr.(i) in
      if c = red then print_string "red : "
      else if c = blue then print_string "blue : "
      else print_string "green : ";
      List.iter (fun (c, n) ->
        if c = red then print_string "(red, "
        else if c = blue then print_string "(blue, "
        else print_string "(white, ";
        print_int n;
        print_string ") "
      ) l;
      print_newline () in
    let get_fnc_line i =
      let c, l = arr.(i) in
      let size, r, b, l = List.fold_left
        (fun (s, r, b, l) (c, n) ->
          (s + 1,
            r + (if c = red then 1 else 0),
            b + (if c = blue then 1 else 0),
            if c = white then n :: l else l
          )
        ) (0, 0, 0, []) l in
      if c = green
      then
        (
          let nbR = (size / 2 - r) in
          if size mod 2 = 0 && nbR >= 0
          then Logic.split true nbR l
          else Logic.antilogy
        )
      else
        let boo = (c = red) in
        let k = size / 2 + 1 - (
          if boo then r else b
        ) in
        if k < 0
        then Logic.tautology
        else Logic.at_least boo k l
    in
    let fnc = ref [] in
    iter_area (fun n c _ ->
        if c = white
        then fnc := [(true, n); (false, n)] :: !fnc
        else ()
      ) bsp 0. 0.;
    for i = 0 to Array.length arr - 1 do
      let f = get_fnc_line i in
      print_aja i;
      print_fnc f;
      print_newline ();
      fnc := List.rev_append f !fnc
    done;
    !fnc

  let get_solution bsp = SOLVER.solve (get_fnc bsp)

  let get_clue bsp =
    let sol = get_solution bsp in
    match sol with
    | None -> print_endline "No solution."; None
    | Some s ->
      print_endline "There is a solution.";
      let size = List.length s in
      if size > 0
      then
        let n = Random.int size in
        let (b, i) = List.nth s n in
        let c = if b then red else blue in
        Some (i, c)
      else None

  let has_solution bsp = (get_solution bsp) <> None

  let is_solution bsp =
    let rec is_full bsp =
      match bsp with
      | L (_, _, c, left, right) ->
        c <> white && is_full left && is_full right
      | _ -> true
    in
    if is_full bsp
    then true
    else
      let arr = get_lines_area bsp (get_number_lines bsp) in
      Array.for_all (fun (c, l) ->
        let r, b = List.fold_left
          (fun (r, b) (c, n) ->
            ((if c = red then 1 else 0) + r, (if c = blue then 1 else 0) + b)
          ) (0, 0) l in
          if c = red then r > b
          else if c = blue then r < b
          else r = b
      ) arr

end
