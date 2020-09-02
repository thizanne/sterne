open Physics.Quantity

type t

val empty : t
val full_tanks : Tank.t list -> t
val set_tank_full : Tank.t -> t -> t

val remaining_normal_volume : Tank.t -> t -> normal_volume
(** Undefined behaviour if the tank isn't part of the supply *)

val remaining_pressure : Tank.t -> t -> pressure
(** Undefined behaviour if the tank isn't part of the supply *)

val breathe_on_segment : < sac:Gas_consumption.sac; .. > -> Profile.Segment.t -> t -> t
val breathe_on_profile : < sac:Gas_consumption.sac; .. > -> Profile.t -> t -> t
val breathe_on_profile' : < sac:Gas_consumption.sac; .. > -> Profile.t -> t
