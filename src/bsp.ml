open Graphics
open Geometry

module type Bsp_type = sig

  type bsp
  
  val change_color : bsp -> point -> bsp

  val generate_random_bsp : float -> float -> bsp

  val iter_area : (Graphics.color -> point list -> unit) -> bsp -> float -> float -> unit

  val iter_line : (line -> unit) -> bsp -> float -> float -> unit

end

let _ = Random.self_init ()

module Bsp_extrem : Bsp_type = struct

  type bsp = R of Graphics.color | L of line * bsp * bsp

  let rec insert bound_x bound_y bsp line =
    let rec aux bsp line pts =
      match bsp with
      | L (l, left, right) ->
         begin
           let left_pts, right_pts = split_by_line l pts in
           match intersect line l with
           | None ->
              if is_left line.pt1 l
              then L (l, aux left line left_pts, right)
              else L (l, left, aux right line right_pts)
           | Some pt ->
              if dist pt line.pt1 > 2. && dist pt line.pt2 > 2.
              then
                let ptl, ptr =
                  if is_left line.pt1 l
                  then line.pt1, line.pt2
                  else line.pt2, line.pt1
                in
                let linel = {pt1 = pt; pt2 = ptl} in
                let liner = {pt1 = pt; pt2 = ptr} in
                L (l, aux left linel left_pts, aux right liner right_pts)
              else
                if is_left line.pt1 l && is_left line.pt2 l
                then L (l, aux left line left_pts, right)
                else L (l, left, aux right line right_pts)
         end
      | r -> L (line, r, r)
    in
    let pts, lines = edges bound_x bound_y in
    aux bsp line pts

  let iter_area f bsp bound_x bound_y =
    let rec find_polygone bsp pts =
      match bsp with
      | L (l, left, right) ->
         let left_pts, right_pts = split_by_line l pts in
         find_polygone left left_pts;
         find_polygone right right_pts;
         ()
      | R c ->
         let barycenter = center pts in
         let pts = List.sort (compare_counter_clockwise barycenter) pts in
         f c pts
    in
    let pts, lines = edges bound_x bound_y in
    find_polygone bsp pts

  let rec change_color bsp pt =
    match bsp with
    | L (l, left, right) ->
       let left, right =
         if is_left pt l
         then change_color left pt, right
         else left, change_color right pt
       in
       L (l, left, right)
    | R r ->
       if r = white
       then R red
       else if r = red
       then R blue
       else if r = blue
       then R white
       else failwith "not a valid color"

  let rec generate_random_bsp bound_x bound_y =
    let vertices, bounds = edges bound_x bound_y in
    let rec gen_ij () =
      let i, j = Random.int 4, Random.int 4 in
      if i = j then gen_ij () else i, j in
    let i, j = gen_ij () in
    let di, dj = List.nth bounds i, List.nth bounds j in
    let first_line = {
        pt1 = point_on_line di (Random.float 1.);
        pt2 = point_on_line dj (Random.float 1.);
      } in
    let p = Random.float 1. in
    gen_bsp p bounds first_line

  and point_on_line l p =
    {
      x = l.pt1.x *. p +. (1. -. p) *. l.pt2.x;
      y = l.pt1.y *. p +. (1. -. p) *. l.pt2.y;
    }

  and find_line p_pt orth acc bound =
    match intersect_lines bound orth with
    | None -> acc
    | Some pt ->
       if dist p_pt pt < 2.
       then acc
       else pt
      
  and new_bounds line (bl, br) l =
    match intersect_lines line l with
    | None ->
       if is_left l.pt1 line && is_left l.pt2 line
       then l :: bl, br
       else bl, l :: br
    | Some pt ->
       let pl, pr =
         if is_left l.pt1 line
         then l.pt1, l.pt2
         else l.pt2, l.pt1
       in
       let left, right =
         {pt1=pl;pt2=pt},
         {pt1=pr;pt2=pt}
       in
       left :: bl, right :: br
       
  and increment p bounds l =
    let p_pt = point_on_line l p in
    let orth = {
        pt1 = p_pt;
        pt2 = {
            x = l.pt2.y -. l.pt1.y;
            y = l.pt1.x -. l.pt1.x
          }
      } in
    let new_line = {
        pt1 = p_pt;
        pt2 = List.fold_left (find_line p_pt orth) p_pt bounds
      } in
    let bl, br =
      List.fold_left (new_bounds new_line) ([new_line], [new_line]) bounds in
    L (l, gen_bsp p bl orth, gen_bsp (1. -. p) br orth)

  and gen_bsp p bounds l =
    let region =
      List.fold_left
        (fun pts line ->
          print_line line; print_string "\n"; flush stdout;
          match List.exists (fun pt -> dist pt line.pt1 < 2.) pts, List.exists (fun pt -> dist pt line.pt2 < 2.) pts with
          | true, true -> pts
          | false, true -> line.pt1 :: pts
          | true, false -> line.pt2 :: pts
          | false, false -> line.pt2 :: line.pt1 :: pts)
        [] bounds in
    let c = center region in
    let pts = List.sort (compare_counter_clockwise c) region in
    let a = area pts in
    print_endline (string_of_float a);
    if a <= 500000.
    then R white
    else increment p bounds l

  let rec iter_line f bsp bound_x bound_y =
    match bsp with
    | L (l, left, right) ->
       f l;
       iter_line f left bound_x bound_y;
       iter_line f right bound_x bound_y
    | R color -> ()

end

module Bsp_classic : Bsp_type = struct

  type bsp = L of float * bsp * bsp | R of Graphics.color

  let change_color bsp pt = 
    let rec change_color_depth bsp pt depth =
      match bsp with
      | L (v, left, right) ->
         let is_left =
           if depth mod 2 = 0
           then pt.x < v
           else pt.y < v in
         if is_left
         then L(v, change_color_depth left pt (depth + 1), right)
         else L(v, left, change_color_depth right pt (depth + 1))
      | R r ->
         if r = white
         then R red
         else if r = red
         then R blue
         else if r = blue
         then R white
         else failwith "not a valid color"      
    in
    change_color_depth bsp pt 0

  exception ToSmallArea
  let min_area = 50.
  let area_range = min_area /. 5.
    
  let generate_random_bsp bound_x bound_y =
    let rec gen_while max min =
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
             L (x,
                generate_random_bsp_depth pt1 {x = x; y = pt2.y} (depth + 1),
                generate_random_bsp_depth {x = x; y = pt1.y} pt2 (depth + 1))
        else let y = gen_rand pt2.y pt1.y in
             L (y,
                generate_random_bsp_depth pt1 {x = pt2.x; y = y} (depth + 1),
                generate_random_bsp_depth {x = pt1.x; y = y} pt2 (depth + 1))
      with _ -> R white
    in
    generate_random_bsp_depth {x = 0.; y = 0.} {x = bound_x; y = bound_y} 0

  let iter_area f bsp bound_x bound_y =
    let rec iter_area_depth f bsp pt1 pt2 depth =
      match bsp with
      | L (v, left, right) ->
         if depth mod 2 = 0
         then begin
             iter_area_depth f left pt1 {x = v; y = pt2.y} (depth + 1);
             iter_area_depth f right {x = v; y = pt1.y} pt2 (depth + 1)
           end
         else begin
             iter_area_depth f left pt1 {x = pt2.x; y = v} (depth + 1);
             iter_area_depth f right {x = pt1.x; y = v} pt2 (depth + 1)
           end
      | R color ->
         f color [pt1; {x = pt2.x; y = pt1.y}; pt2; {x = pt1.x; y = pt2.y}]
    in
    iter_area_depth f bsp {x = 0.; y = 0.} {x = bound_x; y = bound_y} 0

  let iter_line f bsp bound_x bound_y =
    let rec iter_line_depth f bsp pt1 pt2 depth =
      match bsp with
      | L (v, left, right) ->
         if depth mod 2 = 0
         then begin
             f {pt1 = {x = v; y = pt1.y}; pt2 = {x = v; y= pt2.y}};
             iter_line_depth f left pt1 {x = v; y = pt2.y} (depth + 1);
             iter_line_depth f right {x = v; y = pt1.y} pt2 (depth + 1)
           end
         else begin
             f {pt1 = {x = pt1.x; y = v}; pt2 = {x = pt2.x; y = v}};
             iter_line_depth f left pt1 {x = pt2.x; y = v} (depth + 1);
             iter_line_depth f right {x = pt1.x; y = v} pt2 (depth + 1)
           end
      | R color -> ()
    in
    iter_line_depth f bsp {x = 0.; y = 0.} {x = bound_x; y = bound_y} 0
end
