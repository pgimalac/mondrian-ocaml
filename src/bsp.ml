open Graphics

type point = { x : float; y : float; }
type line = { pt1 : point; pt2 : point; }
type bsp = R of Graphics.color | L of line * bsp * bsp

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
  in if xTheta = yTheta
     then 0
     else if xTheta > yTheta
     then 1
     else -1

let area pts =
  match pts with
  | _ :: _ :: [] | _ :: [] | [] -> failwith "Not a polygone"
  | pt1 :: pts ->
     let rec aux acc pts =
       match pts with
       | a :: b :: q ->
          aux (acc +. a.x *. b.y -. a.y *. b.x) (b :: q)
       | a :: [] -> a.x *. pt1.y -. a.y *. pt1.x +. acc
       | [] -> failwith "not a polygone"
     in aux 0. pts

let edges w h =
  let e1, e2, e3, e4 =
    {x=0.; y=0.}, {x=w; y=0.}, {x=0.; y=h}, {x=w; y=h}
  in [e1; e2; e3; e4], [
      {pt1=e1;pt2=e2};
      {pt1=e1;pt2=e3};
      {pt1=e2;pt2=e4};
      {pt1=e3;pt2=e4}
    ]

let split_by_line l pts =
  List.fold_left (fun (pts_l, pts_r) pt ->
      if is_left pt l
      then if is_right pt l
           then pt :: pts_l, pt :: pts_r
           else pt :: pts_l, pts_r
      else pts_l, pt :: pts_r)
    ([l.pt1; l.pt2], [l.pt1; l.pt2]) pts

let iter f bsp bound_x bound_y =
  let rec find_polygone bsp pts =
    match bsp with
    | L (l, left, right) ->
       let left_pts, right_pts = split_by_line l pts in
       find_polygone left left_pts;
       find_polygone right right_pts;
       ()
    | R c -> f c pts
  in
  let pts, lines = edges bound_x bound_y in
  find_polygone bsp pts

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

let rec print_point pt =
  print_string ("("^(string_of_float pt.x)^","^(string_of_float pt.y)^")");
  flush stdout

let rec print_line line =
  print_point line.pt1;
  print_string (" - ");
  print_point line.pt2;
  print_string "\n";
  flush stdout

let rec print_bsp bsp =
  match bsp with
  | L (line, l, r) ->
     print_line line;
     print_endline "left";
     print_bsp l;
     print_endline "right";
     print_bsp r
  | R c ->
     print_endline "area"

let rec change_color bsp pt =
  match bsp with
  | L (l, left, right) ->
     let left, right =
       if is_left pt l
       then change_color left pt, right
       else left, change_color right pt
     in
     L (l, left, right)
  | R r ->
     if r = white
     then R red
     else if r = red
     then R blue
     else if r = blue
     then R white
     else failwith "not a valid color"

let _ = Random.self_init ()

let rec generate_random_bsp_maxime_dont_work bound_x bound_y =
  let vertices, bounds = edges bound_x bound_y in
  let rec gen_ij () =
    let i, j = Random.int 4, Random.int 4 in
    if i = j then gen_ij () else i, j in
  let i, j = gen_ij () in
  let di, dj = List.nth bounds i, List.nth bounds j in
  let first_line = {
      pt1 = point_on_line di (Random.float 1.);
      pt2 = point_on_line dj (Random.float 1.);
    } in
  let p = Random.float 1. in
  let bsp = gen_bsp p bounds first_line in
  print_bsp bsp;
  bsp

and point_on_line l p =
  {
    x = l.pt1.x *. p +. (1. -. p) *. l.pt2.x;
    y = l.pt1.y *. p +. (1. -. p) *. l.pt2.y;
  }

and find_line p_pt orth acc bound =
  match intersect_lines bound orth with
  | None -> acc
  | Some pt ->
     if dist p_pt pt < 2.
     then acc
     else pt
  
and new_bounds line (bl, br) l =
  match intersect_lines line l with
  | None ->
     if is_left l.pt1 line && is_left l.pt2 line
     then l :: bl, br
     else bl, l :: br
  | Some pt ->
     let pl, pr =
       if is_left l.pt1 line
       then l.pt1, l.pt2
       else l.pt2, l.pt1
     in
     let left, right =
       {pt1=pl;pt2=pt},
       {pt1=pr;pt2=pt}
     in
     left :: bl, right :: br
    
and increment p bounds l =
  let p_pt = point_on_line l p in
  let orth = {
      pt1 = p_pt;
      pt2 = {
          x = l.pt2.y -. l.pt1.y;
          y = l.pt1.x -. l.pt1.x
        }
    } in
  let new_line = {
      pt1 = p_pt;
      pt2 = List.fold_left (find_line p_pt orth) p_pt bounds
    } in
  let bl, br =
    List.fold_left (new_bounds new_line) ([new_line], [new_line]) bounds in
  L (l, gen_bsp p bl orth, gen_bsp (1. -. p) br orth)

and gen_bsp p bounds l =
  let region =
    List.fold_left
      (fun pts line ->
        print_line line; print_string "\n"; flush stdout;
        match List.exists (fun pt -> dist pt line.pt1 < 2.) pts, List.exists (fun pt -> dist pt line.pt2 < 2.) pts with
        | true, true -> pts
        | false, true -> line.pt1 :: pts
        | true, false -> line.pt2 :: pts
        | false, false -> line.pt2 :: line.pt1 :: pts)
      [] bounds in
  let c = center region in
  let pts = List.sort (compare_counter_clockwise c) region in
  let a = area pts in
  print_endline (string_of_float a);
  if a <= 500000.
  then R white
  else increment p bounds l
