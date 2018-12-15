(** Return game page for given game settings*)
val make_game_view : (module Settings.Game_settings) -> Interface.page
