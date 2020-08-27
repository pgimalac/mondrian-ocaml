(** Implementation of the Bsp_complete
    module for the extrem mode *)

module Bsp : functor (S : Settings.Game_settings) (C : Settings.Colors) ->
  Bsp.Bsp_complete
