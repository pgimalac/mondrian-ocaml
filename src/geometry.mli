type point = { x : float; y : float; }
type line = { pt1 : point; pt2 : point; }
val draw_line : Graphics.color -> int -> line -> unit
val coefs : line -> (float * float) option
val is_left : point -> line -> bool
val is_right : point -> line -> bool
val intersect_lines : line -> line -> point option
val intersect : line -> line -> point option
val center : point list -> point
val dist : point -> point -> float
val find_angle : point -> float
val compare_counter_clockwise : point -> point -> point -> int
val polygon_area : point list -> float
val edges : float -> float -> point list * line list
val separate_points :
  line -> point list * point list -> point list -> point list * point list
val split_by_line : line -> point list -> point list * point list
val print_point : point -> unit
val print_line : line -> unit
val gen_dot_on_line : line -> point
val gen_random_lines : point array -> line
