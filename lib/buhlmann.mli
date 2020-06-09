open Physics.Quantity

(** {1 Bühlmann decompression algorithm} *)

val deco_procedure : Param.t -> (fraction * fraction) -> Dive.t -> Dive.Profile.t
(** [deco_procedure (gf_low, gf_high) bottom_profile] computes the
    decompression profile for a dive. *)
