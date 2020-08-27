(** Implementation of the complete bsp module
    for the classic mode *)

module Bsp : functor (S : Settings.Game_settings) (C : Settings.Colors) ->
  Bsp.Bsp_complete
