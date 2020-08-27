module Make : functor (S : Settings.Game_settings) -> sig
  val antilogy : (bool * (int * Graphics.color)) list list

  val tautology : (bool * (int * Graphics.color)) list list

  val get_function_color :
    Graphics.color ->
    int ->
    int ->
    int ->
    int ->
    int list ->
    (bool * (int * Graphics.color)) list list

  val basics : int -> (bool * (int * Graphics.color)) list list
end
