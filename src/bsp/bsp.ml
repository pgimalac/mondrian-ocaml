open Graphics
open Geometry
open Sat_solver

module SOLVER = Make (struct
  type t = int
  let compare a b = a - b
end)

module type Bsp_type = sig

  type bsp

  val change_color : ?reverse:bool -> bsp -> point -> bsp

  val generate_random_bsp : float -> float -> bsp * int

  val region : region_label -> bsp

  val node : line_label -> bsp -> bsp -> bsp

  val fold : float -> float ->
             (line_label -> 'a -> 'a -> 'a) ->
             (region_label -> 'a) ->
             bsp -> 'a

  val iter : float -> float ->
             (line_label -> 'a -> 'a * 'a) ->
             (region_label -> 'a -> unit) ->
             'a -> bsp -> unit

end

module type Bsp_complete = sig

  include Bsp_type

  val iter_area : (region_label -> point list -> unit) ->
                  bsp -> float -> float -> unit

  val iter_line : (line_label -> unit) -> bsp -> float -> float -> unit

  val clean : float -> float -> bsp -> bsp

  val get_lines_area : float -> float -> bsp -> int -> int list array

  val init : float -> float -> bsp -> bsp

  val colors : float -> float -> bsp -> int array

  val color_nth : float -> float ->
                  bsp -> int -> color ->
                  bsp

  val get_fnc : float -> float ->
                int list array -> bsp ->
                (bool * int) list list

  val get_solution : float -> float ->
                     int list array -> bsp ->
                     (bool * int) list option

  val get_clue : float -> float ->
                 int list array -> bsp ->
                 (int * color) option

  val has_solution : float -> float ->
                     int list array -> bsp ->
                     bool

  val is_solution : float -> float ->
                    int list array -> bsp ->
                    bool

end

module Make (B : Bsp_type) = struct

  include B

  let iter_area f bsp bound_x bound_y =
    let pts, _ = edges bound_x bound_y in
    iter bound_x bound_y
      (fun l pts -> split_by_line l.section pts)
      (fun r pts ->
         let barycenter = center pts in
         let pts = List.sort (compare_counter_clockwise barycenter) pts in
         f r pts)
      pts
      bsp

  let iter_line f bsp bound_x bound_y =
    iter bound_x bound_y
      (fun l () -> f l; (), ())
      (fun _ _ -> ())
      ()
      bsp

  let for_all_lines bound_x bound_y pred bsp =
    fold bound_x bound_y
      (fun line left right -> pred line && left && right)
      (fun _ -> true)
      bsp

  let clean bound_x bound_y bsp =
    fold bound_x bound_y
      node
      (fun r -> region {r with color = white})
      bsp

  let get_lines_area bound_x bound_y bsp number_lines =
    let arr = Array.make number_lines [] in
    iter bound_x bound_y
      (fun label lines ->
        separate_lines label.section ([label], [label]) lines)
      (fun region lines ->
        List.iter
          (fun (l: line_label) -> arr.(l.id) <- region.id :: arr.(l.id))
          lines)
      [] bsp;
    arr

  let init bound_x bound_y bsp =
    let i = ref (-1) in
    let j = ref (-1) in
    let t =
      fold bound_x bound_y
        (fun l left right ->
          j := !j + 1;
          node {l with id = !j} left right)
        (fun r ->
          i := !i + 1;
          region {r with id = !i})
        bsp
    in
    t

  let colors bound_x bound_y bsp =
    let colors =
      fold bound_x bound_y
        (fun _ left right -> right @ left)
        (fun region -> [region.color])
        bsp
    in
    Array.of_list colors

  let color_nth bound_x bound_y bsp id color =
    fold bound_x bound_y
      node
      (fun r ->
        if r.id = id
        then region {r with color = color}
        else region r)
    bsp

  let print_fnc f = (* for debug purpose *)
    List.iter (fun x ->
        print_string "[";
        List.iter (fun (b, n) ->
            if not b
            then print_string "not ";
            print_int n; print_string " ") x;
        print_string "] et ";
      ) f;
    print_newline ()

  let get_fnc bound_x bound_y adjacency bsp =
    let region_colors = colors bound_x bound_y bsp in
    let get_fnc_line (line: line_label) =
      let regions = adjacency.(line.id) in
      let size, r, b, l =
        List.fold_left
          (fun (s, r, b, l) id ->
            let color = region_colors.(id) in
            s + 1,
            r + (if color = red then 1 else 0),
            b + (if color = blue then 1 else 0),
            if color = white then id :: l else l)
          (0, 0, 0, []) regions in
      if line.color = green
      then
          let nbR = (size / 2 - r) in
          if size mod 2 = 0 && nbR >= 0
          then Logic.split true nbR l
          else Logic.antilogy
      else
        let boo = (line.color = red) in
        let k = size / 2 + 1 - (if boo then r else b) in
        if k < 0
        then Logic.tautology
        else Logic.at_least boo k l
    in
    fold bound_x bound_y
      (fun line left right ->
        get_fnc_line line
        |> List.rev_append left
        |> List.rev_append right)
      (fun r ->
        if r.color = white
        then [[(true, r.id); (false, r.id)]]
        else [])
      bsp

  let get_solution bound_x bound_y adjacency bsp =
    SOLVER.solve (get_fnc bound_x bound_y adjacency bsp)

  let get_clue bound_x bound_y adjacency bsp =
    let sol = get_solution bound_x bound_y adjacency bsp in
    match sol with
    | None -> None
    | Some s ->
       let size = List.length s in
       if size > 0 then
         let n = Random.int size in
         let (b, i) = List.nth s n in
         let c = if b then red else blue in
         Some (i, c)
       else None

  let has_solution bound_x bound_y adjacency bsp =
    (get_solution bound_x bound_y adjacency bsp) <> None

  let is_solution bound_x bound_y adjacency bsp =
    let is_full bsp =
      fold bound_x bound_y
        (fun line l r -> l && r)
        (fun region -> region.color != white)
        bsp
    in
    if is_full bsp
    then true
    else
      let region_colors = colors bound_x bound_y bsp in
      for_all_lines bound_x bound_y
        (fun line ->
          let r, b =
            List.fold_left
              (fun (r, b) id ->
                let c = region_colors.(id) in
                (if c = red then 1 else 0) + r, (if c = blue then 1 else 0) + b)
              (0, 0) adjacency.(line.id) in
          if line.color = red then r > b
          else if line.color = blue then r < b
          else r = b)
        bsp

end

let _ = Random.self_init ()

let colors = [white; red; blue]
let nb_color = (List.length colors) - 1

let next_color reverse c =
  let rec aux tab =
    match tab with
    | hd1 :: hd2 :: tl when hd1 = c -> hd2
    | hd :: [] when hd = c -> List.hd colors
    | hd :: tl -> aux tl
    | [] -> aux colors
  in
  let rec aux_r tab =
    match tab with
    | hd1 :: hd2 :: tl when hd2 = c -> hd1
    | hd :: tl -> aux_r tl
    | [] -> List.nth colors nb_color
  in (if reverse then aux_r else aux) colors

let rand_color () =
  List.nth colors ((Random.int nb_color) + 1)
