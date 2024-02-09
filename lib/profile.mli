open Physics.Quantity

module Segment : sig
  type t

  val start_depth : t -> depth
  val end_depth : t -> depth
  val duration : t -> depth
  val start_runtime : t -> time_span
  val end_runtime : t -> time_span
  val gas : t -> Gas.t
  val tank : t -> Tank.t
  val is_gas_switch : t -> bool
  val is_deco_transition : t -> bool
end

val from_steps : step list -> Profile.t
(** Version with durations vs. runtimes? Inconsistent runtimes would
    be "fixed" with 0-duration segments instead of <0. Or
    raising... Call the right version depending on how the last point
    was edited? *)

val deco_procedure : #param -> working Profile.t -> deco Profile.t

val combine : working Profile.t -> deco Profile.t -> complete Profile.t
(** Can't directly add deco in main profile: need to have deco parts
    available separately eg. for minimum gas. Do we even need
    a combined type at all or simply a double-fold? *)

val final_depth : t -> depth
(** Required for initial deco ascent *)

val final_tank : t -> tank
(** Required for initial deco ascent *)

(* *)
val zero : runtime:time_span -> _ t
val deco_start : runtime:time_span -> _ t (* bottom runtime or zero? *)
val stay_for : 'a t -> time:time_span -> tank:Tank.t -> 'a t
val stay_until : t -> runtime:time_span -> tank:Tank.t -> t
val ascend : < ascent_speed : depth ; .. > -> t -> depth:depth -> tank:Tank.t -> t
val descend : < descent_speed : depth ; .. > -> t -> depth:depth -> tank:Tank.t -> t
val square : t

(* *)

val fold_segments : f:('accum -> Segment.t -> 'accum) -> init:'accum -> t -> 'accum

module Point : sig
  type t
end

type point = Point.t
type t

val square :
  < descent_speed : depth ; .. > -> tank:Tank.t -> depth:depth -> runtime:time_span -> t
(** A square profile, composed of a descent segment to the specified
    depth and a flat bottom segment. [runtime] is the total time of
    the profile. The behaviour is unspecified if the needed descent
    time is greater than the total time. *)

val append : t -> t -> t
(** Append two profiles by concatenating their segments. *)

val stay_for : t -> time:time_span -> tank:Tank.t -> t
val stay_until : t -> runtime:time_span -> tank:Tank.t -> t
val ascend : < ascent_speed : depth ; .. > -> t -> depth:depth -> tank:Tank.t -> t
val descend : < descent_speed : depth ; .. > -> t -> depth:depth -> tank:Tank.t -> t

val fold_segments :
  f:
    (initial_depth:depth ->
    final_depth:depth ->
    time:time_span ->
    tank:Tank.t ->
    'accum ->
    'accum) ->
  init:'accum ->
  t ->
  'accum

val fold_points : f:('accum -> point -> 'accum) -> init:'accum -> t -> 'accum

(** {2 Pretty printing} *)

val to_strings : ?display_short:bool -> t -> string list list
(** Returns a list of list of strings, each sublist describing
    a segment as [direction; depth; duration; runtime; gas] *)

val pp : ?display_short:bool -> Format.formatter -> t -> unit
(** If [display_short] is [false], the segments shorter than a minute
    are not displayed, unless they also include a gas change. The
    runtime will still take them into account (and can thus be larger
    than the sum of the printed segment times). *)
