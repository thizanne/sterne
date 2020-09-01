open Physics.Quantity

(** {1 Bühlmann decompression algorithm} *)

val deco_procedure : Param.t -> (fraction * fraction) -> Tank.t list -> Profile.t -> Profile.t
(** [deco_procedure (gf_low, gf_high) bottom_profile] computes the
    decompression profile for a dive. *)
