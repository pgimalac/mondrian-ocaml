open Graphics
open Geometry
open Bsp

module Make (C : Settings.Colors) : Bsp_type = struct

  type bsp = R of region_label | L of line_label * bsp * bsp

  let fold _ _ f g bsp =
    let rec aux bsp =
      match bsp with
      | L (l, left, right) -> f l (aux left) (aux right)
      | R a -> g a
    in
    aux bsp

  let iter _ _ f g acc0 bsp =
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
    | R region ->
       R {
           region_id = region.region_id;
           region_color= (C.next_color reverse region.region_color)
         }

  let rec nb = ref 0

  and add_random_line localBsp pts localDepth min_area =
    if localDepth = 0
    then localBsp
    else
      match localBsp with
      | L (l, left, right) ->
         let leftPts, rightPts = split_by_line l.section pts
         in if Random.float 1. < 0.5
            then L(l, add_random_line left leftPts (localDepth - 1) min_area, right)
            else L(l, left, add_random_line right rightPts (localDepth - 1) min_area)
      | R _ ->
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
             L({line_color = 0; line_id = 0; section = new_line},
                R {region_color = C.rand_color (); region_id = 0},
                R {region_color = C.rand_color (); region_id = 0})
           end
         else localBsp

  and gen_random_bsp width height nb_lines maxDepth min_area =
    if maxDepth >= 0 && (2. ** (float_of_int maxDepth)) >= (float_of_int nb_lines /. 2.)
    then R {region_id = 0; region_color = C.rand_color ()}
    else
      let bsp = ref (R {region_id = 0; region_color = white}) in
      let v, _ = edges width height in
      for _ = 1 to nb_lines do
        bsp := add_random_line !bsp v maxDepth min_area
      done;
      !bsp

  and generate_random_bsp width height min_area =
    let bsp = gen_random_bsp width height 100 (-1) (float_of_int min_area) in
    bsp, !nb
end

module Bsp (S : Settings.Game_settings) (C : Settings.Colors) = Bsp.Make(S)(Make(C))
