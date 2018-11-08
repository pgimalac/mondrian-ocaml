type color = Blue | Red
type point = { x : float; y : float; }
type line = { pt1 : point; pt2 : point; c : color; }
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
       let x = (b -. b') /. (a -. a') in
       let y = a *. x +. b in
       Some {x = x; y = y}
     
  
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
          let ptl, ptr =
            if line.pt1.x <= pt.x
            then line.pt1, line.pt2
            else line.pt2, line.pt1
          in
          let linel = {pt1 = pt; pt2 = ptl; c = line.c} in
          let liner = {pt1 = pt; pt2 = ptr; c = line.c} in
          if pt = ptl
          then if pt = ptr
               then L (l, left, right)
               else L (l, left, insert right liner)
          else if pt = ptr
          then L (l, insert left linel, right)
          else L (l, insert left linel, insert right liner)
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
  | R(c) ->
     match c with
     | None -> R (Some (Red))
     | Some (Red) -> R (Some (Blue))
     | Some (Blue) -> R (Some (Red))

