open Physics.Quantity

(** {1 Bühlmann decompression algorithm} *)

val plan_deco :
  < ascent_speed : quantity ; ppo2_max_deco : pressure ; .. > ->
  fraction * fraction ->
  Tank.t list ->
  Profile.t ->
  Profile.t
(** [plan_deco param (gf_low, gf_high) bottom_profile] computes the decompression profile
    for a dive. *)
