open Graphics
open Geometry
open Sat_solver

module SOLVER = Make (struct
  type t = int * color
  let compare (i1, c1) (i2, c2) =
    if i1 = i2 then c1 - c2
    else i1 - i2
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
                (bool * (int * color)) list list

  val get_solution : float -> float ->
                     int list array -> bsp ->
                     (bool * (int * color)) list option

  val get_clue : float -> float ->
                 int list array -> bsp ->
                 (int * color) option

  val has_solution : float -> float ->
                     int list array -> bsp ->
                     bool

  val is_solution : float -> float ->
                    int list array -> bsp ->
                    bool

  val find_center : bsp -> float -> float -> int -> point option

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
    fold bound_x bound_y
      (fun l left right ->
        j := !j + 1;
        node {l with id = !j} left right)
      (fun r ->
        i := !i + 1;
        region {r with id = !i})
      bsp

  let colors bound_x bound_y bsp =
    let size = ref 0 in
    iter_area (fun _ _ -> size := !size + 1) bsp bound_x bound_y;
    let colors = Array.make !size white in
    iter_area (fun region _ -> colors.(region.id) <- region.color) bsp bound_x bound_y;
    colors

  let color_nth bound_x bound_y bsp id color =
    fold bound_x bound_y
      node
      (fun r ->
        if r.id = id
        then region {r with color = color}
        else region r)
    bsp

  let print_color color = (* for debug purpose *)
    print_string (
      if color = white then "white"
      else if color = black then "black"
      else if color = red then "red"
      else if color = green then "green"
      else if color = blue then "blue"
      else if color = yellow then "yellow"
      else if color = cyan then "cyan"
      else if color = magenta then "magenta"
      else "unknown color"
    )

  let print_adjacency (line:line_label) colors adjacency = (* for debug purpose *)
    print_color line.color;
    print_string " : ";
    List.iter (fun i ->
      print_string "(";
      print_int i;
      print_string ", ";
      print_color colors.(i);
      print_string ") ";
    ) adjacency.(line.id);
    print_newline ()

  let print_fnc f = (* for debug purpose *)
    List.iter (fun x ->
        print_string "[";
        List.iter (fun (b, (n, c)) ->
            print_int n;
            print_string " ";
            if not b
            then print_string "not ";
            print_color c; print_string " ou ") x;
        print_string "] et ";
      ) f;
    print_newline ()

  let get_fnc bound_x bound_y adjacency bsp =
    let region_colors = colors bound_x bound_y bsp in
    let get_fnc_line (line:line_label) =
      let size, r, g, b, l =
        List.fold_left
          (fun (s, r, g, b, l) id ->
            let color = region_colors.(id) in
            s + 1,
            r + (if color = red then 1 else 0),
            g + (if color = green then 1 else 0),
            b + (if color = blue then 1 else 0),
            if color = white then id :: l else l)
          (0, 0, 0, 0, []) adjacency.(line.id) in
      let f = Logic.get_function_color line.color in
      f size r g b l
    in
    let basic = Logic.basics () in
    let fnc = ref [] in
    iter bound_x bound_y
      (fun line _ ->
        fnc := List.rev_append (get_fnc_line line) !fnc;
        (0,0)
      ) (fun r _ ->
        if r.color = white
        then fnc := List.rev_append (basic r.id) !fnc
      ) 0 bsp;
    print_endline "done"; flush stdout;
    !fnc

  let get_solution bound_x bound_y adjacency bsp =
    get_fnc bound_x bound_y adjacency bsp
    |> SOLVER.solve

  let get_clue bound_x bound_y adjacency bsp =
    let sol = get_solution bound_x bound_y adjacency bsp in
    match sol with
    | None -> None
    | Some s ->
      let size, sol = List.fold_left (fun (s,l) (b, (i, col)) ->
        if b then (s + 1, (i, col) :: l) else (s, l)
      ) (0, []) s in
      if size > 0 then
        let n = Random.int size in
        Some (List.nth sol n)
      else None

  let has_solution bound_x bound_y adjacency bsp =
    (get_solution bound_x bound_y adjacency bsp) <> None

  let is_solution bound_x bound_y adjacency bsp =
    let is_full bsp =
      fold bound_x bound_y
        (fun _ l r -> l && r)
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
(*
  val iter_area : (region_label -> point list -> unit) ->
                  bsp -> float -> float -> unit
*)

  let find_center bsp bound_x bound_y n =
    let opt = ref None in
    iter_area
      (fun label pts ->
        if label.id = n
        then opt := Some (center pts)
      ) bsp bound_x bound_y;
      !opt
end

let colors = [white; red; green; blue]
let nb_colors = List.length colors - 1

let rec head l =
  match l with
  | a :: b :: q -> a :: (b :: q |> head)
  | _ -> []

let index color =
  let colors =
    if Logic.get_three_colors ()
    then colors
    else head colors
  in
  let rec iter acc l = match l with
    | h :: q ->
      if h = color
      then Some acc
      else iter (acc + 1) q
    | [] -> None
  in iter 0 colors

let next_color reverse c =
  let colors =
    if Logic.get_three_colors ()
    then colors
    else head colors
  in
  let nb_colors =
    if Logic.get_three_colors ()
    then nb_colors
    else nb_colors - 1
  in
  let rec aux tab =
    match tab with
    | hd1 :: hd2 :: _ when hd1 = c -> hd2
    | hd :: [] when hd = c -> List.hd colors
    | _ :: tl -> aux tl
    | [] -> aux colors
  in
  let rec aux_r tab =
    match tab with
    | hd1 :: hd2 :: _ when hd2 = c -> hd1
    | _ :: tl -> aux_r tl
    | [] -> List.nth colors nb_colors
  in (if reverse then aux_r else aux) colors

let rand_color () =
  let nb_colors =
    if Logic.get_three_colors ()
    then nb_colors
    else nb_colors - 1
  in List.nth colors ((Random.int nb_colors) + 1)

let _ =
  Random.self_init ();
  Logic.set_three_colors false

