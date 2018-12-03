open Graphics
open Geometry
   
module type Bsp_type = sig

  type bsp

  val change_color : ?reverse:bool -> bsp -> point -> bsp

  val generate_random_bsp : float -> float -> bsp * int

  val region : region_label -> bsp

  val node : line_label -> bsp -> bsp -> bsp
    
  val fold : float -> float ->
             (line_label -> 'a -> 'a -> 'a) ->
             (region_label -> point list -> 'a) ->
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
end

module Make (B : Bsp_type) = struct

  include B

  let iter_area f bsp bound_x bound_y =
    fold bound_x bound_y (fun _ _ _ -> ()) f bsp

  let iter_line f bsp bound_x bound_y =
    fold bound_x bound_y (fun l _ _ -> f l) (fun _ _ -> ()) bsp

  let clean bound_x bound_y bsp =
    fold bound_x bound_y
      node
      (fun r _ -> region {r with color = white})
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
      (fun r _ ->
        i := !i + 1;
        region {r with id = !i})
      bsp

  let colors bound_x bound_y bsp =
    let colors = 
      fold bound_x bound_y
        (fun _ left right -> left @ right)
        (fun region _ -> [region.color])
        bsp
    in
    Array.of_list colors

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
