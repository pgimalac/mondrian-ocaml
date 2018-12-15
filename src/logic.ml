open Settings

module Make (S : Game_settings) = struct

  let tautology = []
  let antilogy = [[(true, (-1, Graphics.black))]; [(false, (-1, Graphics.black))]]

  (* all subsets of l of size k *)
  (* TODO: rewrite pure *)
  let subsets f k l =
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
    !e

  let basics_three n =
    [
      (* at least one of the three is be true *)
      [(true, (n, Graphics.red)); (true, (n, Graphics.green)); (true, (n, Graphics.blue))];
      (* at most one of the three is true *)
      [(false, (n, Graphics.red)); (false, (n, Graphics.green))];
      [(false, (n, Graphics.red)); (false, (n, Graphics.blue))];
      [(false, (n, Graphics.green)); (false, (n, Graphics.blue))]
    ]

  let basics_two n =
    [
      (* at least one of the two is be true *)
      [(true, (n, Graphics.red)); (true, (n, Graphics.green))];
      (* at most one of the two is true *)
      [(false, (n, Graphics.red)); (false, (n, Graphics.green))];
    ]

  let basics =
    match S.color with
    | RBColor -> basics_two
    | RGBColor -> basics_three

  (* at least k variables in l are c *)
  let at_least (c:Graphics.color) k l =
    if k <= 0
    then tautology
    else
      let f = (fun (i:int) -> (true, (i, c))) in
      subsets f (List.length l + 1 - k) l

  (* at most k variables in l are c *)
  let at_most (c:Graphics.color) k l =
    if k < 0
    then antilogy
    else
      let f = (fun (i:int) -> (false, (i, c))) in
      subsets f (k + 1) l

  (* red/green/blue means the color is strictly more than half of areas *)
  let one_colored c1 s n1 l =
    let min = s / 2 + 1 in
    if min <= n1
    then tautology
    else at_least c1 (min - n1) l

  let get_red s r _ _ =
    one_colored Graphics.red s r

  let get_green s _ g _ =
    one_colored Graphics.green s g

  let get_blue s _ _ b =
    one_colored Graphics.blue s b

  (* yellow/cyan/magenta means two colors are at least a quarter of areas and at most an half, the third is strictly less than a quarter *)
  let two_colored c1 c2 c3 s n1 n2 n3 l =
    let min = (s + 3) / 4 in
    let max = s / 2 in
    if max < n1 || max < n2 || min - 1 < n3
    then antilogy
    else
      let x1min = at_least c1 (min - n1) l in
      let x2min = at_least c2 (min - n2) l in
      let x1max = at_most c1 (max - n1) l in
      let x2max = at_most c2 (max - n2) l in
      let x3max = at_most c3 (min - 1 - n3) l in
      List.rev_append x2max x3max
      |> List.rev_append x1max
      |> List.rev_append x2min
      |> List.rev_append x1min

  let get_yellow s r g b =
    two_colored Graphics.red Graphics.green Graphics.blue s r g b

  let get_yellow_two s r g _ l =
    let min = (s + 1) / 2 in
    let x1min = at_least Graphics.red (min - r) l in
    let x2min = at_least Graphics.green (min - g) l in
    List.rev_append x1min x2min

  let get_magenta s r g b =
    two_colored Graphics.red Graphics.blue Graphics.green s r b g

  let get_cyan s r g b =
    two_colored Graphics.green Graphics.blue Graphics.red s g b r

  let get_white s r g b l =
    let min = (s + 3) / 4 in
    let max = s / 2 in
    if max < r || max < g || max < b
    then antilogy
    else
      let rmin = at_least Graphics.red (min - r) l in
      let gmin = at_least Graphics.green (min - g) l in
      let bmin = at_least Graphics.blue (min - b) l in
      List.rev_append gmin bmin
      |> List.rev_append rmin

  let get_black _ _ _ _ _ = tautology

  let get_function_color color =
    if color = Graphics.white then get_white
    else if color = Graphics.black then get_black
    else if color = Graphics.red then get_red
    else if color = Graphics.green then get_green
    else if color = Graphics.blue then get_blue
    else if color = Graphics.magenta then get_magenta
    else if color = Graphics.cyan then get_cyan
    else if color = Graphics.yellow then begin
        match S.color with
        | RBColor -> get_yellow_two
        | RGBColor -> get_yellow
      end
    else failwith "get_function_color : unknown color"
end
