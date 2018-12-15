open Graphics

type game_mode = Classic | Extrem
type color_mode = RGColor | RGBColor

module type Game_settings = sig
  val mode : game_mode
  val color : color_mode
  val min_area : int
  val black_probability : int
end

let game_colors = [white; red; green; blue]

let rec head l =
  match l with
  | a :: b :: q -> a :: (b :: q |> head)
  | _ -> []

let _ =
  Random.self_init ();

module type Colors = sig
  val index : Graphics.color -> int option
  val next_color : bool -> Graphics.color -> Graphics.color
  val rand_color : unit -> Graphics.color
end

module Make_Colors (S : Game_settings) = struct

  let nb_colors =
    match S.color with
    | RGColor -> 2
    | RGBColor -> 3

  let game_colors =
    match S.color with
    | RGBColor -> game_colors
    | RGColor -> head game_colors

  let index color =
    let rec iter acc l =
      match l with
      | h :: q ->
         if h = color
         then Some acc
         else iter (acc + 1) q
      | [] -> None
    in iter 0 game_colors

  let next_color reverse c =
    let rec aux tab =
      match tab with
      | hd1 :: hd2 :: _ when hd1 = c -> hd2
      | hd :: [] when hd = c -> List.hd game_colors
      | _ :: tl -> aux tl
      | [] -> aux game_colors
    in
    let rec aux_r tab =
      match tab with
      | hd1 :: hd2 :: _ when hd2 = c -> hd1
      | _ :: tl -> aux_r tl
      | [] -> List.nth game_colors nb_colors
    in (if reverse then aux_r else aux) game_colors

  let rand_color () =
    List.nth game_colors ((Random.int nb_colors) + 1)

end
