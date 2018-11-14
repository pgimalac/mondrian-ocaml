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

let area barycenter pts =
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
      else pts_l, pt :: pts_r
    ) ([l.pt1; l.pt2], [l.pt1; l.pt2]) pts

let iter f bsp bound_x bound_y =
  let rec find_polygone bsp pts =
    match bsp with
    | L (l, left, right) ->
       let left_pts, right_pts = split_by_line l pts in
       find_polygone left left_pts;
       find_polygone right right_pts
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
         | Some(pt) ->
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
    | r ->
       let barycenter = center pts in
       if area barycenter
            (List.sort (compare_counter_clockwise barycenter) pts)
          >= 50000. (* some random min area value *)
       then L (line, r, r)
       else r
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
    print_bsp l;
    print_bsp r
  | R c -> ()

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

let generate_random_bsp bound_x bound_y nb_line =
  let nb_line = nb_line + 4 in
  let rec gen_random_lines acc lines bound =
     if bound >= nb_line
     then acc
     else
       let i = Random.int bound in
       let j = Random.int bound in
       if i = j
       then gen_random_lines acc lines bound
       else
        let d_i = List.nth lines i in
        let d_j = List.nth lines j in
        let gen_dot_on_line line =
          match coefs line with
          | None ->
            {
              x = line.pt1.x;
              y = Random.float (abs_float (line.pt1.y -. line.pt2.y)) +. (min line.pt1.y line.pt2.y)
            }
          | Some (a, b) ->
            let x = Random.float (abs_float (line.pt1.x -. line.pt2.x)) +. (min line.pt1.x line.pt2.x)
            in {x = x; y = a *. x +. b}
        in let new_line = {
          pt1=gen_dot_on_line d_i;
          pt2=gen_dot_on_line d_j
        }
        in
        gen_random_lines (new_line :: acc) (new_line :: lines) (bound + 1)
  in
  let _, lines = edges bound_x bound_y in
  let lines = List.rev (gen_random_lines [] lines 4) in
  let bsp = List.fold_left (insert bound_x bound_y) (R white) lines
  in bsp
