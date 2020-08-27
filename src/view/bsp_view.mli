val menu_page : Interface.page option ref

val make_game_view : (module Settings.Game_settings) -> Interface.page
(** Return game page for given game settings*)
