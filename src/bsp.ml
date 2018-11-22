open Ratio

type point = { x : ratio; y : ratio; }
type line = { pt1 : point; pt2 : point; }
type bsp = R of Graphics.color | L of line * bsp * bsp

let min_area = 10000.

let rec print_point pt =
  print_string ("("^(string_of_ratio pt.x)^","^(string_of_ratio pt.y)^")");
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

let coefs line =
  let dy = line.pt2.y -@ line.pt1.y in
  let dx = line.pt2.x -@ line.pt1.x in
  if dx = ratio0
  then None
  else
    let a = dy /@ dx in
    let b = line.pt1.y -@ a *@ line.pt1.x in
    Some (a, b)

let is_left pt line =
  match coefs line with
  | None -> pt.x <=@ line.pt1.x
  | Some (a, b) -> a *@ pt.x +@ b >=@ pt.y

let is_right pt line =
  match coefs line with
  | None -> pt.x >=@ line.pt1.x
  | Some (a, b) -> a *@ pt.x +@ b <=@ pt.y


let intersect_lines l1 l2 = (* intersect lines *)
  match coefs l1, coefs l2 with
  | None, None ->
     if l1.pt1.x = l2.pt1.x
     then failwith "Same lines"
     else None
  | Some (a, b), None -> Some {x = l2.pt1.x; y = a *@ l2.pt1.x +@ b}
  | None, Some (a, b) -> Some {x = l2.pt1.x; y = a *@ l2.pt1.x +@ b}
  | Some (a, b), Some (a', b') ->
     if a = a'
     then if b = b'
        then failwith "Same lines"
        else None
     else
       let x = (b' -@ b) /@ (a -@ a') in
       let y = a *@ x +@ b in
       Some {x=x; y=y}

let intersect l1 l2 = (* intersect segments *)
  match coefs l1, coefs l2 with
  | None, None ->
     if l1.pt1.x = l2.pt1.x
     then failwith "Same lines"
     else None
  | Some (a, b), None ->
    if (l1.pt1.x -@ l2.pt1.x) *@ (l1.pt2.x -@ l2.pt1.x) <=@ ratio0 &&
       (l1.pt1.y -@ l2.pt1.y) *@ (l1.pt2.y -@ l2.pt1.y) <=@ ratio0
    then Some {x = l2.pt1.x; y = a *@ l2.pt1.x +@ b}
    else None
  | None, Some (a, b) ->
    if (l2.pt1.x -@ l1.pt1.x) *@ (l2.pt2.x -@ l1.pt1.x) <=@ ratio0 &&
       (l2.pt1.y -@ l1.pt1.y) *@ (l2.pt2.y -@ l1.pt1.y) <=@ ratio0
    then Some {x = l2.pt1.x; y = a *@ l2.pt1.x +@ b}
    else None
  | Some (a, b), Some (a', b') ->
     if a = a'
     then if b = b'
        then failwith "Same lines"
        else None
     else
       let x = (b' -@ b) /@ (a -@ a') in
       let y = a *@ x +@ b in
       if (x -@ l1.pt1.x) *@ (x -@ l1.pt2.x) <=@ ratio0 &&
          (x -@ l2.pt1.x) *@ (x -@ l2.pt2.x) <=@ ratio0 &&
          (y -@ l1.pt1.y) *@ (y -@ l1.pt2.y) <=@ ratio0 &&
          (y -@ l2.pt1.y) *@ (y -@ l2.pt2.y) <=@ ratio0
       then Some {x=x; y=y}
       else None

let center pts =
  let l, sx, sy =
    List.fold_left
      (fun (l, sx, sy) pt -> l +@ ratio1, pt.x +@ sx, pt.y +@ sy)
      (ratio0, ratio0, ratio0) pts
  in {x = sx /@ l; y = sy /@ l}

let dist a b =
  let dx = float_of_ratio (a.x -@ b.x) in
  let dy = float_of_ratio (a.y -@ b.y) in
  sqrt (dx *. dx +. dy *. dy)

let find_angle p =
  if p.x = ratio0 && p.y = ratio0 then 0.
  else
    let r = dist p { x = ratio0; y = ratio0 }
    in let p'x = (float_of_ratio p.x) /. r
    in let p'y = (float_of_ratio p.y) /. r
    in let theta = acos p'x
    in if asin p'y < 0.
       then -.theta
       else theta

let compare_counter_clockwise center a b =
  let aTheta = find_angle {x = a.x -@ center.x ; y = a.y -@ center.y}
  in let bTheta = find_angle {x = b.x -@ center.x ; y = b.y -@ center.y}
  in compare aTheta bTheta

let polygon_area v =
  match v with
  | [] | _ :: [] | _ :: _ :: [] -> 0.
  | h :: q ->
    let rec aux l =
      match l with
      | a :: b :: q -> a.x *@ b.y -@ a.y *@ b.x +@ aux (b :: q)
      | [a] -> a.x *@ h.y -@ a.y *@ h.x
      | [] -> ratio0 (* to prevent warnings... *)
    in (float_of_ratio (aux v)) /. 2.

let separate_points l =
  List.fold_left (fun (pts_l, pts_r) pt ->
               if is_left pt l
               then if is_right pt l
                    then pt :: pts_l, pt :: pts_r
                    else pt :: pts_l, pts_r
               else pts_l, pt :: pts_r
             )

let edges w h =
  let e1, e2, e3, e4 =
    {x=ratio0; y=ratio0}, {x=w; y=ratio0}, {x=ratio0; y=h}, {x=w; y=h}
  in [e1; e2; e3; e4], [
      {pt1=e1;pt2=e2};
      {pt1=e1;pt2=e3};
      {pt1=e2;pt2=e4};
      {pt1=e3;pt2=e4}
    ]

let iter f bsp bound_x bound_y =
  let rec find_polygone bsp pts =
    match bsp with
    | L (l, left, right) ->
       let left_pts, right_pts = separate_points l ([l.pt1;l.pt2],[l.pt1;l.pt2]) pts in
       find_polygone left left_pts;
       find_polygone right right_pts;
    | R c -> f c pts
  in
  let pts, lines = edges bound_x bound_y in
  find_polygone bsp pts

let rec insert bound_x bound_y bsp line =
  let rec aux bsp line pts =
    match bsp with
    | L (l, left, right) ->
       begin
         let left_pts, right_pts = separate_points l ([],[]) pts in
         match intersect line l with
         | None ->
            if is_left line.pt1 l
            then L (l, aux left line left_pts, right)
            else L (l, left, aux right line right_pts)
         | Some pt ->
            if pt != line.pt1 && pt != line.pt2
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
     if c = Graphics.white then R Graphics.red
     else if c = Graphics.red then R Graphics.blue
     else if c = Graphics.blue then R Graphics.white
     else failwith "not a valid color"

let _ = Random.self_init ()

let gen_dot_on_line line =
  let p = random_ratio 1. in
  {x = line.pt1.x +@ p *@ (line.pt2.x -@ line.pt1.x); y = line.pt1.y +@ p *@ (line.pt2.y -@ line.pt1.y)}

let gen_random_lines ptsArr =
  let length = Array.length ptsArr in
  let i = Random.int length in
  let j = Random.int (length - 1) in
  let j = if i <= j then j + 1 else j in
  let d_i = {pt1 = ptsArr.(i); pt2 = ptsArr.((i + 1) mod length)} in
  let d_j = {pt1 = ptsArr.(j); pt2 = ptsArr.((j + 1) mod length)} in
  {pt1 = gen_dot_on_line d_i ; pt2 = gen_dot_on_line d_j}

let generate_random_bsp width height nb_lines maxDepth =
  if maxDepth >= 0 && (2. ** (float_of_int maxDepth)) >= (float_of_int nb_lines /. 2.)
  then R Graphics.white
  else
    let bsp = ref (R Graphics.white) in
    let v, _ = edges width height in
    let rec add_random_line localBsp pts localDepth =
      if localDepth = 0
      then localBsp
      else
        match localBsp with
        | L (l, left, right) ->
          let leftPts, rightPts = separate_points l ([l.pt1; l.pt2], [l.pt1; l.pt2]) pts
          in if Random.float 1. < 0.5
             then L(l, add_random_line left leftPts (localDepth - 1), right)
             else L(l, left, add_random_line right rightPts (localDepth - 1))
        | R c ->
          let pts = List.sort (compare_counter_clockwise (center pts)) pts
          in let ptsArr = Array.of_list pts
          in let new_line = gen_random_lines ptsArr
          in let left, right = separate_points new_line ([new_line.pt1; new_line.pt2], [new_line.pt1; new_line.pt2]) pts
          in let left = List.sort (compare_counter_clockwise (center left)) left
          in let right = List.sort (compare_counter_clockwise (center right)) right
          in if polygon_area left > min_area && polygon_area right > min_area
             then L(new_line, R c, R c)
             else localBsp
    in for _ = 1 to nb_lines do
      bsp := add_random_line !bsp v maxDepth
    done;
    print_bsp !bsp;
    !bsp

let rec generate_random_bsp_maxime_dont_work bound_x bound_y =
  let vertices, bounds = edges bound_x bound_y in
  let rec gen_ij () =
    let i, j = Random.int 4, Random.int 4 in
    if i = j then gen_ij () else i, j in
  let i, j = gen_ij () in
  let di, dj = List.nth bounds i, List.nth bounds j in
  let first_line = {
      pt1 = point_on_line di (random_ratio 1.);
      pt2 = point_on_line dj (random_ratio 1.);
    } in
  let p = random_ratio 1. in
  let bsp = gen_bsp p bounds first_line in
  print_bsp bsp;
  bsp

and point_on_line l p =
  {
    x = l.pt1.x *@ p +@ (ratio1 -@ p) *@ l.pt2.x;
    y = l.pt1.y *@ p +@ (ratio1 -@ p) *@ l.pt2.y;
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
          x = l.pt2.y -@ l.pt1.y;
          y = l.pt1.x -@ l.pt1.x
        }
    } in
  let new_line = {
      pt1 = p_pt;
      pt2 = List.fold_left (find_line p_pt orth) p_pt bounds
    } in
  let bl, br =
    List.fold_left (new_bounds new_line) ([new_line], [new_line]) bounds in
  L (l, gen_bsp p bl orth, gen_bsp (ratio1 -@ p) br orth)

and gen_bsp p bounds l =
  let region =
    List.fold_left
      (fun pts line ->
        print_line line; print_string "\n"; flush stdout;
        match List.mem line.pt1 pts, List.mem line.pt2 pts with
        | true, true -> pts
        | false, true -> line.pt1 :: pts
        | true, false -> line.pt2 :: pts
        | false, false -> line.pt2 :: line.pt1 :: pts)
      [] bounds in
  let c = center region in
  let pts = List.sort (compare_counter_clockwise c) region in
  let a = polygon_area pts in
  print_endline (string_of_float a);
  if a <= 500000.
  then R Graphics.white
  else increment p bounds l
