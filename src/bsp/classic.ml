open Graphics
open Geometry
open Bsp

module Bsp_classic : Bsp_type = struct
  type bsp = L of float * bsp * bsp | R of Graphics.color

  let change_color ?(reverse=false) bsp pt =
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
      | R r -> R (next_color reverse r)
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

  let rec clean bsp =
    match bsp with
    | L (f, left, right) ->
       L (f, clean left, clean right)
    | R _ -> R white
end
