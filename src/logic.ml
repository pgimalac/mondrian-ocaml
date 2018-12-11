let antilogy = [[(true, (0, Graphics.red))]; [(false, (0, Graphics.red))]]
let tautology = []

(* all subsets of l of size k *)
let subsets f k l =
(* not tail-recursive but heap space linear to the size of l *)
(* so less than the number of areas -- so ok *)
  let e = ref [] in
  let l =
    List.fold_left (fun acc h ->
        List.fold_left (fun acc (n, l) ->
            if n = k
            then begin
                e := l :: !e;
                acc
              end
            else (n + 1, f h :: l) :: (n, l) :: acc
          ) [] acc
      ) [(0, [])] l
  in
  List.iter (fun (n, l) -> if n = k then e := l :: !e) l;
  if !e = [] then antilogy else !e

let basics n =
  [
   (* at least one of the three is be true *)
   [(true, (n, Graphics.red)); (true, (n, Graphics.green)); (true, (n, Graphics.blue))];
   (* at most one of the three is true *)
   [(false, (n, Graphics.red)); (false, (n, Graphics.green))];
   [(false, (n, Graphics.red)); (false, (n, Graphics.blue))];
   [(false, (n, Graphics.green)); (false, (n, Graphics.blue))]
  ]

(* at least k variables in l are c *)
let at_least (c:Graphics.color) k l =
  let f = (fun (i:int) -> (true, (i, c))) in
  subsets f (List.length l + 1 - k) l

(* at most k variables in l are c *)
let at_most (c:Graphics.color) k l =
  let f = (fun (i:int) -> (false, (i, c))) in
  subsets f (k + 1) l

let one_colored c1 s n1 l =
  let min = (s + 1) / 2 in
  if min <= n1
  then tautology
  else at_least c1 (min - n1) l

let get_red s r _ _ =
  one_colored Graphics.red s r

let get_green s _ g _ =
  one_colored Graphics.green s g

let get_blue s _ _ b =
  one_colored Graphics.blue s b

(* c1 and c2 are the main colors, c3 the third one *)
let two_colored c1 c2 c3 s n1 n2 n3 l =
  let min = (s + 3) / 4 in
  let max = s / 2 in
  if max < n1 || max < n2 || min - 1 < n3
  then antilogy
  else
    let x1min = if min <= n1 then tautology else at_least c1 (min - n1) l in
    let x2min = if min <= n2 then tautology else at_least c2 (min - n2) l in
    let x1max = at_most c1 (max - n1) l in
    let x2max = at_most c2 (max - n2) l in
    let x3max = at_most c3 (min - 1 - n3) l in
    List.rev_append x1min (
      List.rev_append x1max (
        List.rev_append x2min (
          List.rev_append x2max x3max
        )
      )
    )

let get_yellow s r g b =
  two_colored Graphics.red Graphics.green Graphics.blue s r g b

let get_magenta s r g b =
  two_colored Graphics.red Graphics.blue Graphics.green s r b g

let get_cyan s r g b =
  two_colored Graphics.green Graphics.blue Graphics.red s g b r

let three_colored c1 c2 c3 min1 max1 min2 max2 min3 max3 l =
  if max1 < 0 || max2 < 0 || max3 < 0 || min1 > max1 || min2 > max2 || min3 > max3
  then antilogy
  else
    let x1min = if min1 <= 0 then tautology else at_least c1 min1 l in
    let x2min = if min2 <= 0 then tautology else at_least c2 min2 l in
    let x3min = if min3 <= 0 then tautology else at_least c3 min3 l in
    let x1max = at_most c1 max1 l in
    let x2max = at_most c2 max2 l in
    let x3max = at_most c3 max3 l in
    List.rev_append x1min (
      List.rev_append x2min (
        List.rev_append x3min (
          List.rev_append x1max (
            List.rev_append x2max x3max
          )
        )
      )
    )

let get_white s r g b l =
  let min = (s + 3) / 4 in
  let max = (s + 1) / 2 in
  three_colored Graphics.red Graphics.green Graphics.blue
    (min - r) (max - r) (min - g) (max - g) (min - b) (max - b) l

let get_black _ _ _ _ _ = tautology

let get_function_color color =
  if color = Graphics.white then get_white
  else if color = Graphics.black then get_black
  else if color = Graphics.red then get_red
  else if color = Graphics.green then get_green
  else if color = Graphics.blue then get_blue
  else if color = Graphics.yellow then get_yellow
  else if color = Graphics.magenta then get_magenta
  else if color = Graphics.cyan then get_cyan
  else failwith "get_function_color : unknown color"
