open Graphics

type point = { x : float; y : float; }
type line = { pt1 : point; pt2 : point; }

type line_label = {
    id      : int;
    section : line;
    color   : color;
  }

let change_label label line =
  {
    id = label.id;
    color = label.color;
    section = line
  }
                
type region_label = {
    id : int;
    color : color;
  }

          
let draw_line color width l =
  Graphics.set_line_width width;
  Graphics.set_color color;
  moveto (int_of_float l.pt1.x) (int_of_float l.pt1.y);
  lineto (int_of_float l.pt2.x) (int_of_float l.pt2.y)

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
  | Some (a, b) -> a *. pt.x +. b >= pt.y

let is_right pt line =
  match coefs line with
  | None -> pt.x >= line.pt1.x
  | Some (a, b) -> a *. pt.x +. b <= pt.y

let intersect_lines l1 l2 =
  match coefs l1, coefs l2 with
  | None, None ->
     if l1.pt1.x = l2.pt1.x
     then failwith "Same lines"
     else None
  | Some (a, b), None -> Some {x = l2.pt1.x; y = a *. l2.pt1.x +. b}
  | None, Some (a, b) -> Some {x = l2.pt1.x; y = a *. l2.pt1.x +. b}
  | Some (a, b), Some (a', b') ->
     if a = a'
     then if b = b'
        then failwith "Same lines"
        else None
     else
       let x = (b' -. b) /. (a -. a') in
       let y = a *. x +. b in
       Some {x=x; y=y}

let intersect l1 l2 =
  match coefs l1, coefs l2 with
  | None, None ->
     if l1.pt1.x = l2.pt1.x
     then failwith "Same lines"
     else None
  | Some (a, b), None ->
    if (l1.pt1.x -. l2.pt1.x) *. (l1.pt2.x -. l2.pt1.x) <= 0. &&
       (l1.pt1.y -. l2.pt1.y) *. (l1.pt2.y -. l2.pt1.y) <= 0.
    then Some {x = l2.pt1.x; y = a *. l2.pt1.x +. b}
    else None
  | None, Some (a, b) ->
    if (l2.pt1.x -. l1.pt1.x) *. (l2.pt2.x -. l1.pt1.x) <= 0. &&
       (l2.pt1.y -. l1.pt1.y) *. (l2.pt2.y -. l1.pt1.y) <= 0.
    then Some {x = l2.pt1.x; y = a *. l2.pt1.x +. b}
    else None
  | Some (a, b), Some (a', b') ->
     if a = a'
     then if b = b'
        then failwith "Same lines"
        else None
     else
       let x = (b' -. b) /. (a -. a') in
       let y = a *. x +. b in
       if (x -. l1.pt1.x) *. (x -. l1.pt2.x) <= 0. &&
          (x -. l2.pt1.x) *. (x -. l2.pt2.x) <= 0. &&
          (y -. l1.pt1.y) *. (y -. l1.pt2.y) <= 0. &&
          (y -. l2.pt1.y) *. (y -. l2.pt2.y) <= 0.
       then Some {x=x; y=y}
       else None

let center pts =
  let l, sx, sy =
    List.fold_left
      (fun (l, sx, sy) pt -> l +. 1., pt.x +. sx, pt.y +. sy)
      (0., 0., 0.) pts
  in {x = sx /. l; y = sy /. l}

let dist x y =
  sqrt ((x.x -. y.x) *. (x.x -. y.x) +. (x.y -. y.y) *. (x.y -. y.y))

let find_angle p =
  if p.x = 0. && p.y = 0. then 0.
  else
    let r = dist p {x=0.; y=0.}
    in let p' = {x=p.x /. r; y=p.y /.r}
    in let theta = acos p'.x
    in if asin p'.y < 0.
       then 0. -. theta
       else theta

let compare_counter_clockwise center x y =
  let xTheta = find_angle {x = x.x -. center.x ; y = x.y -. center.y}
  in let yTheta = find_angle {x = y.x -. center.x ; y = y.y -. center.y}
  in compare xTheta yTheta

let polygon_area v =
  match v with
  | [] | _ :: [] | _ :: _ :: [] -> 0.
  | h :: q ->
    let rec aux l =
      match l with
      | a :: b :: q -> a.x *. b.y -. a.y *. b.x +. aux (b :: q)
      | [a] -> a.x *. h.y -. a.y *. h.x
      | [] -> 0. (* to prevent warnings... *)
    in aux v /. 2.

let edges w h =
  let e1, e2, e3, e4 =
    {x=0.; y=0.}, {x=w; y=0.}, {x=0.; y=h}, {x=w; y=h}
  in [e1; e2; e3; e4], [
      {pt1=e1;pt2=e2};
      {pt1=e1;pt2=e3};
      {pt1=e2;pt2=e4};
      {pt1=e3;pt2=e4}
    ]

let is_on_line l pt =
  let ptAux =
    match coefs l with
    | Some (a, b) -> { x = pt.x; y = a *. pt.x +. b}
    | None -> { x = l.pt1.x; y = pt.y }
  in dist pt ptAux < 2.

let separate_points l =
  List.fold_left
    (fun (pts_l, pts_r) pt ->
      if is_left pt l
      then if is_right pt l
           then pt :: pts_l, pt :: pts_r
           else pt :: pts_l, pts_r
      else pts_l, pt :: pts_r)

let split_by_line l pts =
  separate_points l ([l.pt1; l.pt2], [l.pt1; l.pt2]) pts

let separate_lines l  =
  List.fold_left
    (fun (left, right) label ->
      let pt1, pt2 = label.section.pt1, label.section.pt2 in
      if is_left pt1 l && is_left pt2 l
      then label :: left, right
      else if is_right pt1 l && is_right pt2 l
      then left, label :: right
      else
        let inter =
          if is_on_line label.section l.pt1
          then l.pt1
          else l.pt2 in
        let ptL, ptR =
          if is_left pt1 l
          then pt1, pt2
          else pt2, pt1
        in
        let leftlabel, rightlabel =
          change_label label {pt1 = ptL; pt2 = inter}, 
          change_label label {pt1 = ptR; pt2 = inter} in
        leftlabel :: left, rightlabel :: right)

  
let rec print_point pt =
  print_string ("("^(string_of_float pt.x)^","^(string_of_float pt.y)^")");
  flush stdout

let rec print_line line =
  print_point line.pt1;
  print_string (" - ");
  print_point line.pt2;
  print_string "\n";
  flush stdout

let gen_dot_on_line line =
  let p = Random.float 1.
  in {x = line.pt1.x +. p *. (line.pt2.x -. line.pt1.x); y = line.pt1.y +. p *. (line.pt2.y -. line.pt1.y)}

let gen_random_lines ptsArr =
  let length = Array.length ptsArr in
  let i = Random.int length in
  let j = Random.int (length - 1) in
  let j = if i <= j then j + 1 else j in
  let d_i = {pt1 = ptsArr.(i); pt2 = ptsArr.((i + 1) mod length)} in
  let d_j = {pt1 = ptsArr.(j); pt2 = ptsArr.((j + 1) mod length)} in
  {pt1 = gen_dot_on_line d_i ; pt2 = gen_dot_on_line d_j}
