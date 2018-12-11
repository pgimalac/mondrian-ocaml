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

let one_colored c1 min1 l =
  if min1 <= 0
  then tautology
  else at_least c1 min1 l

let get_red =
  one_colored Graphics.red

let get_green =
  one_colored Graphics.green

let get_blue =
  one_colored Graphics.blue

(* c1 and c2 are the main colors, c3 the third one *)
let two_colored c1 c2 c3 min1 max1 min2 max2 max3 l =
  if max1 < 0 || max2 < 0 || max3 < 0 || min1 > max1 || min2 > max2
  then antilogy
  else
    let x1min = if min1 <= 0 then [] else at_least c1 min1 l in
    let x2min = if min2 <= 0 then [] else at_least c2 min2 l in
    let x1max = at_most c1 max1 l in
    let x2max = at_most c2 max2 l in
    let x3max = at_most c3 max3 l in
    List.rev_append x1min (
      List.rev_append x1max (
        List.rev_append x2min (
          List.rev_append x2max x3max
        )
      )
    )

let get_yellow =
  two_colored Graphics.red Graphics.green Graphics.blue

let get_magenta =
  two_colored Graphics.red Graphics.blue Graphics.green

let get_cyan =
  two_colored Graphics.green Graphics.blue Graphics.red

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

let get_black =
  three_colored Graphics.red Graphics.green Graphics.blue

let number_of_colors c =
  let n = ref 0 in
  if c / 65536 <> 0 then n := !n + 1; (* r *)
  if (c / 256) mod 256 <> 0 then n := !n + 1; (* g *)
  if c mod 256 <> 0 then n := !n + 1; (* b *)
  !n
