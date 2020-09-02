open Physics.Quantity

(** Surface air consumption rate *)
type sac

val liters_per_minute : float -> sac
(** [liters_per_minute 20.] is the SAC of a diver breathing 20 normal
    liters per minute. *)

val breathe_at_depth : sac -> pressure -> time_span -> normal_volume
(** The normal volume breathed at a given ambient pressure during
    a given time span *)

val breathe_on_segment : sac -> Profile.Segment.t -> normal_volume
(** The normal volume breathed during some dive segment *)
