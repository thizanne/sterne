(** {1 Some parameters for diving related computations } *)

(** These parameters are currently set in the source and cannot be
    modified. *)

val ascent_speed : Physics.Quantity.other

val descent_speed : Physics.Quantity.other

val ppo2_max_deco : Physics.Quantity.pressure

val ppo2_max_bottom : Physics.Quantity.pressure
