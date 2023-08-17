open Physics.Quantity

(** {1 Breathing gases } *)

type t [@@deriving sexp]
(** Type of breathing gases. *)

(** Elements composing a breathing gas *)
type element =
  | O2
  | He
  | N2

val equal : t -> t -> bool
val ( = ) : t -> t -> bool
val ( <> ) : t -> t -> bool

val fraction : element -> t -> fraction
(** Returns the fraction of the element in the gas. *)

(** {2 Create gases and basic functions } *)

val air : t
val oxy : t
val nitrox : o2:percentage -> t
val nitrox' : o2:fraction -> t
val nx : percentage_int -> t
val trimix : o2:percentage -> he:percentage -> t
val trimix' : o2:fraction -> he:fraction -> t

val tx : percentage_int -> percentage_int -> t
(** [tx o2 he] *)

val tx' : percentage_int * percentage_int -> t
(** [tx' (o2, he)] *)

val heliair : o2:percentage -> t
val is_air : t -> bool
val is_oxy : t -> bool
val is_nitrox : t -> bool
val partial_pressure : element -> t -> depth -> pressure

(** {3 Diving related functions } *)

val mod_ : ?ppo2_max:pressure -> t -> depth

val end_ : t -> depth -> depth
(** Conservative: assumes that oxygen is as narcotic as nitrogen. *)

val ead : t -> depth -> depth
(** Equivalent air depth for saturation. Fails is gas is not
    nitrox. *)

val is_breathable : ?ppo2_max:pressure -> depth:depth -> t -> bool

(** {2 Pretty printing and parsing } *)

val pp : Format.formatter -> t -> unit
(** Prints as [Air], [Oxy], [Nx 32] or [Tx 18/45]. *)

val parse : string -> t option
(** Parses [air], [oxy], [32] or [18/45]. *)

module Best : sig
  val nitrox : < ppo2_max_bottom : pressure ; .. > -> depth:depth -> unit -> t

  val trimix :
    < ppo2_max_bottom : pressure ; .. > -> ?end_:depth -> depth:depth -> unit -> t
end
