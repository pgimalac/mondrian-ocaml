type color = Blue | Red
type point = { x : float; y : float; }
type line = { pt1 : point; pt2 : point; }
type bsp = R of color option | L of line * bsp * bsp

let pi = 4.0 *. atan 1.0;;

let coefs line =
  let dy = line.pt2.y -. line.pt1.y in
  let dx = line.pt2.x -. line.pt1.x in
  if dx = 0.
  then None
  else
    let a = dy /. dx in
    let b = line.pt1.y -. a *. line.pt1.x in
    Some (a, b)

let is_left pt line =
  match coefs line with
  | None -> pt.x <= line.pt1.x
  | Some (a, b) -> a *. pt.x +. b <= pt.y

let is_right pt line =
  match coefs line with
  | None -> pt.x >= line.pt1.x
  | Some (a, b) -> a *. pt.x +. b >= pt.y

let intersect l1 l2 =
  match coefs l1, coefs l2 with
  | None, None ->
     if l1.pt1.x = l2.pt1.x
     then Some (l1.pt1)
     else None
  | Some (a, b), None ->
     Some {x = l2.pt1.x; y = a *. l2.pt1.x +. b}
  | None, Some (a, b) ->
     Some {x = l1.pt1.x; y = a *. l1.pt1.x +. b}
  | Some (a, b), Some (a', b') ->
     if a = a'
     then if b = b'
          then Some l1.pt1
          else None
     else
       let x = (b' -. b) /. (a -. a') in
       let y = a *. x +. b in
       Some {x=x; y=y}
       
let center pts =
  let l, sx, sy =
    List.fold_left
      (fun (l, sx, sy) pt -> l +. 1., pt.x +. sx, pt.y +. sy)
      (0., 0., 0.) pts
  in {x = sx /. l; y = sy /. l}
              
let compare_counter_clockwise center x y =
  let f =
    if (x.x -. center.x) *. (y.x -. center.x) >= 0.
    then (x.x -. center.x) *. (x.y -. y.y)
    else x.x -. y.x
  in int_of_float f

let edges w h =
  let e1, e2, e3, e4 =
    {x=0.; y=0.}, {x=w; y=0.}, {x=0.; y=h}, {x=w; y=h}
  in [e1; e2; e3; e4], [
      {pt1=e1;pt2=e2};
      {pt1=e1;pt2=e3};
      {pt1=e2;pt2=e4};
      {pt1=e3;pt2=e4}
    ] 

let rec insert bsp line =
  match bsp with
  | L (l, left, right) ->
     begin
       match intersect line l with
       | None ->
          if is_left line.pt1 l
          then L (l, insert left line, right)
          else L (l, left, insert right line)
       | Some(pt) ->
          if pt.x >= min l.pt1.x l.pt2.x &&
              pt.x <= max l.pt1.x l.pt2.x &&
                pt.y >= min l.pt1.y l.pt2.y &&
                  pt.y <= max l.pt1.y l.pt2.y
          then
            begin
              let ptl, ptr =
                if line.pt1.x <= pt.x
                then line.pt1, line.pt2
                else line.pt2, line.pt1
              in
              let linel = {pt1 = pt; pt2 = ptl} in
              let liner = {pt1 = pt; pt2 = ptr} in
              if pt = ptl
              then if pt = ptr
                   then L (l, left, right)
                   else L (l, left, insert right liner)
              else if pt = ptr
              then L (l, insert left linel, right)
              else L (l, insert left linel, insert right liner)
            end
          else
            if is_left line.pt1 l
            then L (l, insert left line, right)
            else L (l, left, insert right line)
     end
  | r -> L (line, r, r)

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
     match c with
     | None -> R (Some Red)
     | Some Red -> R (Some Blue)
     | Some Blue -> R (Some Red)

let _ = Random.self_init ()

let in_bounds bound_min bound_max x =
  max bound_min (max bound_max x)
      
let generate_random_bsp bound_x bound_y nb_line =
  let nb_line = nb_line + 4 in
  let rec gen_random_lines acc lines bound =
    if bound >= nb_line
    then acc
    else
      let in_bounds_y = in_bounds 0. bound_y in
      let i = Random.int bound in
      let j = Random.int bound in
      if i = j
      then gen_random_lines acc lines bound
      else
        let d_i = List.nth lines i in
        let d_j = List.nth lines j in
        let new_line =
          match coefs d_i, coefs d_j with
          | None, None ->
             {
               pt1={x=d_i.pt1.x;y=Random.float bound_y};
               pt2={x=d_j.pt1.x;y=Random.float bound_y}
             }
          | Some (a, b), None ->
             let random_x = Random.float bound_x in
             {
               pt1={x=random_x;y=in_bounds_y (random_x *. a +. b)};
               pt2={x=d_i.pt2.x;y=Random.float bound_y}
             }
          | None, Some (a, b) ->
             let random_x = Random.float bound_x in
             {
               pt1={x=d_i.pt1.x;y=Random.float bound_y};
               pt2={x=random_x;y=in_bounds_y (random_x *. a +. b)};
             }
          | Some (a, b), Some (a', b') ->
             let random_xi = Random.float bound_x in
             let random_xj = Random.float bound_x in
             {
               pt1={x=random_xi;y=in_bounds_y (random_xi *. a +. b)};
               pt2={x=random_xj;y=in_bounds_y (random_xj *. a' +. b')};
             }
        in
        gen_random_lines (new_line :: acc) (new_line :: lines) (bound + 1)
  in
  let _, lines = edges bound_x bound_y in
  let lines = gen_random_lines [] lines 4 in
  List.fold_left insert (R None) lines
