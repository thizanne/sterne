open Physics.Quantity

(** {1 General parameters for diving computations } *)

type t =
  < ascent_speed : other
  ; descent_speed : other
  ; ppo2_max_deco : pressure
  ; ppo2_max_bottom : pressure
  ; sac : Gas_consumption.sac >

val default : t
