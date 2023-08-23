open Physics.Quantity

(** {1 General parameters for diving computations } *)

type t =
  < ascent_speed : quantity
  ; descent_speed : quantity
  ; ppo2_max_deco : pressure
  ; ppo2_max_bottom : pressure
  ; sac : Gas_consumption.sac >

val default : t
