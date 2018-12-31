val menu_page : (Interface.page option) ref

(** Return game page for given game settings*)
val make_game_view : (module Settings.Game_settings) -> Interface.page
