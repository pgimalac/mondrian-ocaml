open Graphics
open Geometry
open Bsp

module Bsp_extrem : Bsp_type = struct
  let min_area = 10000.

  type bsp = R of Graphics.color | L of line * bsp * bsp

  let fold bound_x bound_y f g bsp =
    let rec aux bsp pts =
      match bsp with
      | L (l, left, right) ->
         let left_pts, right_pts = split_by_line l pts in
         let accl = aux left left_pts in
         let accr = aux right right_pts in
         f l accl accr
      | R c ->
         let barycenter = center pts in
         let pts = List.sort (compare_counter_clockwise barycenter) pts in
         g c pts
    in
    let pts, lines = edges bound_x bound_y in
    aux bsp pts

  let region c = R c

  let node line left right = L(line, left, right)
    
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

  let rec change_color reverse bsp pt =
    match bsp with
    | L (l, left, right) ->
       let left, right =
         if is_left pt l
         then change_color reverse left pt, right
         else left, change_color reverse right pt
       in
       L (l, left, right)
    | R c -> R (next_color reverse c)

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

module Bsp = Bsp.Make (Bsp_extrem)
