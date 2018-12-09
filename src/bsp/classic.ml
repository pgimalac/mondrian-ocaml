open Graphics
open Geometry
open Bsp

module Bsp_classic : Bsp_type = struct
  type label = {
      color   : color;
      section : float;
      id      : int;
    }

  type bsp = L of label * bsp * bsp | R of region_label

  let pp v pt1 pt2 depth =
    if depth mod 2 = 0
    then
      {pt1 = {x = v.section; y = pt2.y}; pt2 = {x = v.section; y = pt1.y}},
      (pt1, {x = v.section; y = pt2.y}),
      ({x = v.section; y = pt1.y}, pt2)
    else
      {pt1 = {x = pt2.x; y = v.section}; pt2 = {x = pt1.x; y = v.section}},
      (pt1, {x = pt2.x; y = v.section}),
      ({x = pt1.x; y = v.section}, pt2)

  let change_color ?(reverse=false) bsp pt =
    let rec change_color_depth bsp pt depth =
      match bsp with
      | L (v, left, right) ->
         let is_left =
           if depth mod 2 = 0
           then pt.x < v.section
           else pt.y < v.section in
         if is_left
         then L(v, change_color_depth left pt (depth + 1), right)
         else L(v, left, change_color_depth right pt (depth + 1))
      | R r -> R {r with color = next_color reverse r.color}
    in
    change_color_depth bsp pt 0

  exception ToSmallArea
  let min_area = 50.
  let area_range = min_area /. 5.

  let generate_random_bsp bound_x bound_y =
    let nb = ref 0 in
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
      nb := !nb + 1;
      try
        if depth mod 2 = 0
        then let x = gen_rand pt2.x pt1.x in
             L ({color = 0;section = x; id = 0},
                generate_random_bsp_depth pt1 {x = x; y = pt2.y} (depth + 1),
                generate_random_bsp_depth {x = x; y = pt1.y} pt2 (depth + 1))
        else let y = gen_rand pt2.y pt1.y in
             L ({color = 0;section = y; id = 0},
                generate_random_bsp_depth pt1 {x = pt2.x; y = y} (depth + 1),
                generate_random_bsp_depth {x = pt1.x; y = y} pt2 (depth + 1))
      with _ -> R {id = 0; color = rand_color ()}
    in
    let bsp =
      generate_random_bsp_depth
        {x = 0.; y = 0.} {x = bound_x; y = bound_y} 0
    in
    bsp, !nb

  let region c = R c

  let node (line: line_label) left right =
    if line.section.pt1.x = line.section.pt2.x
    then
      L({section = line.section.pt1.x; id = line.id; color = line.color}, left, right)
    else
      L({section = line.section.pt1.y; id = line.id; color = line.color}, left, right)

  let to_line_label (v: label) line : line_label =
    {
      section = line;
      color = v.color;
      id = v.id;
    }

  let fold bound_x bound_y f g bsp =
    let rec fold_depth bsp (pt1, pt2) depth =
      match bsp with
      | L (v, left, right) ->
         let line, ptl, ptr = pp v pt1 pt2 depth in
         let label = to_line_label v line in
         f label (fold_depth left ptl (depth + 1)) (fold_depth right ptr (depth + 1))
      | R color -> g color
    in
    fold_depth bsp ({x = 0.; y = 0.}, {x = bound_x; y = bound_y}) 0

  let iter bound_x bound_y f g acc0 bsp =
    let rec aux bsp acc (pt1, pt2) depth =
      match bsp with
      | L (v, left, right) ->
         let line, ptl, ptr = pp v pt1 pt2 depth in
         let label = to_line_label v line in
         let accl, accr = f label acc in
         aux left accl ptl (depth + 1);
         aux right accr ptr (depth + 1)
      | R region -> g region acc
    in
    aux bsp acc0 ({x = 0.; y = 0.}, {x = bound_x; y = bound_y}) 0

end

module Bsp = Bsp.Make (Bsp_classic)
