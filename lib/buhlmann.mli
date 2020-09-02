open Physics.Quantity

(** {1 Bühlmann decompression algorithm} *)

val deco_procedure :
  < ascent_speed : other; ppo2_max_deco : pressure; .. > ->
  (fraction * fraction) ->
  Tank.t list ->
  Profile.t ->
  Profile.t
(** [deco_procedure param (gf_low, gf_high) bottom_profile] computes the
    decompression profile for a dive. *)
