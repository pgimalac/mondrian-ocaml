open Ratio

type point = { x : ratio; y : ratio; }
type line = { pt1 : point; pt2 : point; }
type bsp = R of Graphics.color | L of line * bsp * bsp

val min_area : float

val coefs : line -> (ratio * ratio) option

val is_left : point -> line -> bool

val is_right : point -> line -> bool

val iter : (Graphics.color -> point list -> unit) -> bsp -> ratio -> ratio -> unit

val insert : ratio -> ratio -> bsp -> line -> bsp

val intersect : line -> line -> point option

val change_color : bsp -> point -> bsp

val center : point list -> point

val compare_counter_clockwise : point -> point -> point -> int

val edges : ratio -> ratio -> (point list) * (line list)

val generate_random_bsp : ratio -> ratio -> int -> int -> bsp

val generate_random_bsp_maxime_dont_work : ratio -> ratio -> bsp

val print_point : point -> unit

val print_bsp : bsp -> unit

val print_line : line -> unit

val find_angle : point -> float

val dist : point -> point -> float

val separate_points : line -> (point list * point list) -> point list -> (point list * point list)
