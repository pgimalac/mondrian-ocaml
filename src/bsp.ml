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
  let min_area = 10000.
               
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

  let gen_random_lines ptsArr =
    let length = Array.length ptsArr in
    let i = Random.int length in
    let j = Random.int (length - 1) in
    let j = if i <= j then j + 1 else j in
    let d_i = {pt1 = ptsArr.(i); pt2 = ptsArr.((i + 1) mod length)} in
    let d_j = {pt1 = ptsArr.(j); pt2 = ptsArr.((j + 1) mod length)} in
    {pt1 = gen_dot_on_line d_i ; pt2 = gen_dot_on_line d_j}

  let rec add_random_line localBsp pts localDepth =
    if localDepth = 0
    then localBsp
    else
      match localBsp with
      | L (l, left, right) ->
         let leftPts, rightPts =
           separate_points l ([l.pt1; l.pt2], [l.pt1; l.pt2]) pts in
         if Random.float 1. < 0.5
         then L(l, add_random_line left leftPts (localDepth - 1), right)
         else L(l, left, add_random_line right rightPts (localDepth - 1))
      | R c ->
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
                           then L(new_line, R c, R c)
                           else localBsp

    
  and gen_random_bsp width height nb_lines maxDepth =
    if maxDepth >= 0 && (2. ** (float_of_int maxDepth)) >= (float_of_int nb_lines /. 2.)
    then R Graphics.white
    else
      let bsp = ref (R Graphics.white) in
      let v, _ = edges width height in
      for _ = 1 to nb_lines do
        bsp := add_random_line !bsp v maxDepth
      done;
      !bsp
  let rec iter_line f bsp bound_x bound_y =
    match bsp with
    | L (l, left, right) ->
       f l;
       iter_line f left bound_x bound_y;
       iter_line f right bound_x bound_y
    | R color -> ()

  let rec change_color bsp pt =
    match bsp with
    | L (l, left, right) ->
       let left, right =
         if is_left pt l
         then change_color left pt, right
         else left, change_color right pt
       in
       L (l, left, right)
    | R c ->
       if c = Graphics.white then R Graphics.red
       else if c = Graphics.red then R Graphics.blue
       else if c = Graphics.blue then R Graphics.white
       else failwith "not a valid color"

  let rec add_random_line localBsp pts localDepth =
    if localDepth = 0
    then localBsp
    else
      match localBsp with
      | L (l, left, right) ->
         let leftPts, rightPts = separate_points l ([l.pt1; l.pt2], [l.pt1; l.pt2]) pts
         in if Random.float 1. < 0.5
            then L(l, add_random_line left leftPts (localDepth - 1), right)
            else L(l, left, add_random_line right rightPts (localDepth - 1))
      | R c ->
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
         then L(new_line, R c, R c)
         else localBsp
         
  and gen_random_bsp width height nb_lines maxDepth =
    if maxDepth >= 0 && (2. ** (float_of_int maxDepth)) >= (float_of_int nb_lines /. 2.)
    then R Graphics.white
    else
      let bsp = ref (R Graphics.white) in
      let v, _ = edges width height in
      for _ = 1 to nb_lines do
        bsp := add_random_line !bsp v maxDepth
      done;
      !bsp

  let generate_random_bsp width height = gen_random_bsp width height 100 (-1)
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
