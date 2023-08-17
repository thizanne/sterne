(** {1 Physics utilities } *)

(** Physical quantities as types. *)
module Quantity : sig
  (** The goal of this module is to define physical quantities as
      type, to ease their readability. All these quantities are aliases
      to existing types, mostly [float] and [int]: no check is
      performed by the module on the validity of corresponding
      operation. For instance, adding a pressure and a depth will be
      authorised and most probably lead to a programming error.

      Some of these types also add an invariant to their
      specification. Again, no check is performed on the validity of
      these invariants when created by the used. However, functions
      from this library returning this kind of values can be expected
      to fulfill the invariants.

      This module only defines types and is designed to be opened for
      easier definition of types implying quantities.
  *)

  type fraction = float [@@deriving sexp, compare, equal]
  (** A fraction is between [0.] and [1.], both included. *)

  type percentage = float [@@deriving sexp, compare, equal]
  (** A percentage is between [0.] and [100.], both included. *)

  type percentage_int = int [@@deriving sexp, compare, equal]
  (** Sometimes it may be easier to manipulate integers. Between [0]
      and [100]. *)

  type depth = float [@@deriving sexp, compare, equal]
  (** Expressed in meters. *)

  type pressure = float [@@deriving sexp, compare, equal]
  (** Expressed in bar. *)

  type tension = pressure [@@deriving sexp, compare, equal]
  type time_span = Time_float.Span.t [@@deriving sexp, compare, equal]

  type volume = float [@@deriving sexp, compare, equal]
  (** Expressed in cubic meters. *)

  type normal_volume = float [@@deriving sexp, compare, equal]
  (** Has the dimension of [pressure × volume]. Used to describe
      quantities of gas by the volume they would have under a pressure of 1 bar.
  *)

  type dimensionless = float [@@deriving sexp, compare, equal]
  (** For quantities with no dimension nor invariant *)

  type other = float [@@deriving sexp, compare, equal]
  (** For quantities (eg. bar / m) not expressed by other types
      (typically too seldom used to warrant a dedicated name) *)
end

open Quantity

val percent_to_fraction : percentage -> fraction
val fraction_to_percent : fraction -> percentage
val round_fraction_percent_down : fraction -> fraction

val round_fraction_percent_up : fraction -> fraction
(** Round fractions so that they represent an integer percentage *)

val litre : float -> volume

val to_litre : volume -> float
(** When it makes more sense to use litres for a volume. *)

val atmospheric_pressure : pressure
val water_density : dimensionless

val bar_per_meter : other
(** Pressure induced by one meter of water. *)

val depth_to_pressure : depth -> pressure
(** Absolute ambient pressure at some depth *)

val pressure_to_depth : pressure -> depth
(** Depth at which some absolute pressure is ambiant *)

val next_3m_depth : depth -> depth
(** The deeper depth that is both strictly shallower than the [depth]
    parameter and a multiple of 3m *)

val normal_volume_of_gas : pressure:pressure -> volume:volume -> normal_volume
(** Computes the normal volume of a (physical) volume of gas under
    some given pressure. May (and currently does) assume ideal gas law.
*)

val pressure_of_gas : volume:volume -> normal_volume:normal_volume -> pressure
(** The pressure under which the [normal_volume] amount of gas must be
    compressed to fit in [volume]
*)

val volume_of_gas : normal_volume:normal_volume -> pressure:pressure -> volume
(** The volume occupied by the [normal_volume] amount of gas when
    compressed under [pressure].
*)

val pp_time_span : Format.formatter -> time_span -> unit
(** Pretty prints a time span. Round to the second for spans lower
    than a minute, to the minute otherwise. Minutes are printed as
    "min" and not "m". *)
