open Graphics
open Geometry
open Bsp

module Make (C : Settings.Colors) : Bsp_type = struct

  type label = {
      label_color   : color;
      label_section : float;
      label_id      : int;
    }

  type bsp = L of label * bsp * bsp | R of region_label

  let pp v pt1 pt2 depth =
    if depth mod 2 = 0
    then
      {pt1 = {x = v.label_section; y = pt2.y}; pt2 = {x = v.label_section; y = pt1.y}},
      (pt1, {x = v.label_section; y = pt2.y}),
      ({x = v.label_section; y = pt1.y}, pt2)
    else
      {pt1 = {x = pt2.x; y = v.label_section}; pt2 = {x = pt1.x; y = v.label_section}},
      (pt1, {x = pt2.x; y = v.label_section}),
      ({x = pt1.x; y = v.label_section}, pt2)

  let change_color ?(reverse=false) bsp pt =
    let rec change_color_depth bsp pt depth =
      match bsp with
      | L (v, left, right) ->
         let is_left =
           if depth mod 2 = 0
           then pt.x < v.label_section
           else pt.y < v.label_section in
         if is_left
         then L(v, change_color_depth left pt (depth + 1), right)
         else L(v, left, change_color_depth right pt (depth + 1))
      | R r -> R {r with region_color = C.next_color reverse r.region_color}
    in
    change_color_depth bsp pt 0

  exception TooSmallArea
  exception TooDeep

  let generate_random_bsp bound_x bound_y max_depth min_area =
    let min_area = float_of_int min_area in
    let nb = ref 0 in
    let rec gen_while max min =
      let i = (Random.float (max -. min)) +. min in
      if max -. i < 10. || i -. min < 10.
      then gen_while max min
      else i
    and gen_rand max min =
      if (max -. min) *. (max -. min) < min_area
      then raise TooSmallArea
      else gen_while max min
    and generate_random_bsp_depth pt1 pt2 depth =
      nb := !nb + 1;
      try
        if depth = max_depth
        then raise TooDeep
        else if depth mod 2 = 0
        then let x = gen_rand pt2.x pt1.x in
             L ({label_color = 0;label_section = x; label_id = 0},
                generate_random_bsp_depth pt1 {x = x; y = pt2.y} (depth + 1),
                generate_random_bsp_depth {x = x; y = pt1.y} pt2 (depth + 1))
        else let y = gen_rand pt2.y pt1.y in
             L ({label_color = 0;label_section = y; label_id = 0},
                generate_random_bsp_depth pt1 {x = pt2.x; y = y} (depth + 1),
                generate_random_bsp_depth {x = pt1.x; y = y} pt2 (depth + 1))
      with _ -> R {region_id = 0; region_color = C.rand_color ()}
    in
    let bsp =
      generate_random_bsp_depth
        {x = 0.; y = 0.} {x = bound_x; y = bound_y} 0
    in
    bsp, !nb

  let region c = R c

  let node line left right =
    if line.section.pt1.x = line.section.pt2.x
    then
      L({label_section = line.section.pt1.x; label_id = line.line_id; label_color = line.line_color}, left, right)
    else
      L({label_section = line.section.pt1.y; label_id = line.line_id; label_color = line.line_color}, left, right)

  let to_line_label (v: label) line : line_label =
    {
      section = line;
      line_color = v.label_color;
      line_id = v.label_id;
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

module Bsp (S : Settings.Game_settings) (C : Settings.Colors) = Bsp.Make(S)(Make(C))
