open Graphics
open Geometry
open Bsp

module Bsp_extrem : Bsp_type = struct
  let min_area = 10000.
               
  type bsp = R of region_label | L of line_label * bsp * bsp

  let fold bound_x bound_y f g bsp =
    let rec aux bsp pts =
      match bsp with
      | L (l, left, right) ->
         let left_pts, right_pts = split_by_line l.section pts in
         let accl = aux left left_pts in
         let accr = aux right right_pts in
         f l accl accr
      | R a ->
         let barycenter = center pts in
         let pts = List.sort (compare_counter_clockwise barycenter) pts in
         g a pts
    in
    let pts, lines = edges bound_x bound_y in
    aux bsp pts

  let iter bx by f g acc0 bsp =
    let rec aux bsp acc =
      match bsp with
      | L (label, left, right) ->
         let accl, accr = f label acc in
         aux left accl;
         aux right accr
      | R region ->
         g region acc
    in
    aux bsp acc0
       
  let region label = R label

  let node label left right = L(label, left, right)

  let rec change_color ?(reverse=false) bsp pt =
    match bsp with
    | L (l, left, right) ->
       let left, right =
         if is_left pt l.section
         then change_color ~reverse:reverse left pt, right
         else left, change_color ~reverse:reverse right pt
       in
       L (l, left, right)
    | R region -> R {id=region.id;color=(next_color reverse region.color)}

  let rec nb = ref 0

  and add_random_line localBsp pts localDepth =
    if localDepth = 0
    then localBsp
    else
      match localBsp with
      | L (l, left, right) ->
         let leftPts, rightPts = split_by_line l.section pts
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
         in
         let left = List.sort (compare_counter_clockwise (center left)) left in
         let right = List.sort (compare_counter_clockwise (center right)) right in
         if polygon_area left > min_area && polygon_area right > min_area
         then begin
             nb := !nb + 1;
             L({color = 0; id = 0; section = new_line},
                R {color = rand_color (); id = 0},
                R {color = rand_color (); id = 0})
           end
         else localBsp

  and gen_random_bsp width height nb_lines maxDepth =
    if maxDepth >= 0 && (2. ** (float_of_int maxDepth)) >= (float_of_int nb_lines /. 2.)
    then R {id = 0; color = rand_color ()}
    else
      let bsp = ref (R {id = 0; color = white}) in
      let v, _ = edges width height in
      for _ = 1 to nb_lines do
        bsp := add_random_line !bsp v maxDepth
      done;
      !bsp

  and generate_random_bsp width height = gen_random_bsp width height 100 (-1), !nb
end

module Bsp = Bsp.Make (Bsp_extrem)
