open Physics.Quantity

module Segment : sig
  (** Linear segments of dives, with a constant speed.

      Segments are taken in isolation of any actual dive and make sense on their own.
      Therefore, creation functions are available outside a profile.

      In particular, segments contain the needed data for decompression computations while
      still not carrying anchored information that only makes sense within a dive profile.
  *)

  type t

  (** {2 Accessors} *)

  type direction = private
    | Ascent
    | Descent
    | Level

  val start_depth : t -> depth
  val end_depth : t -> depth
  val average_depth : t -> depth
  val duration : t -> time_span
  val tank : t -> Tank.t
  val gas : t -> Gas.t
  val direction : t -> direction

  (** {2 Creation} *)

  val create :
    start_depth:depth -> end_depth:depth -> duration:time_span -> tank:Tank.t -> t

  val stay_for : depth:depth -> duration:time_span -> tank:Tank.t -> t
  val minute_stop : depth:depth -> tank:Tank.t -> t

  val ascent :
    < ascent_speed : speed ; .. > ->
    start_depth:depth ->
    end_depth:depth ->
    tank:Tank.t ->
    t
end

module Leg : sig
  (** Rich, anchored information that dive profile consumers can use for various
      processing purpose. A leg does not make sense on its own but only within the dive
      profile it belongs to, hence no creation function is public. *)

  type t

  type direction = Segment.direction = private
    | Ascent
    | Descent
    | Level

  (** {2 Accessors} *)

  val start_depth : t -> depth
  val end_depth : t -> depth
  val average_depth : t -> depth
  val duration : t -> time_span
  val tank : t -> Tank.t
  val gas : t -> Gas.t
  val direction : t -> direction

  (** The following accessors are what makes legs different from segments: they return
      values that only makes sense because the leg is embedded in a whole profile. *)

  val is_new_tank : t -> bool
  (** {!true} iff. the tank breathed is different from the one breathed in the previous
      leg, or if the leg is the first leg of the profile. *)

  val start_runtime : t -> time_span
  val end_runtime : t -> time_span
end

type t
(** The type of dive profiles. A profile is never empty: it contains at least one leg. *)

(** {2 Accessors} *)

val last_depth : t -> depth
val last_tank : t -> Tank.t

val runtime : t -> time_span
(** Total runtime. *)

(** {2 Construction} *)

val initial_descent :
  < descent_speed : speed ; .. > -> end_depth:depth -> tank:Tank.t -> t

val stay_for : duration:time_span -> tank:Tank.t -> t -> t
val minute_stop : tank:Tank.t -> t -> t

val stay_until :
  runtime:time_span -> tank:Tank.t -> t -> (t, [> `Descent_exceeds_runtime ]) result

val ascend : < ascent_speed : speed ; .. > -> end_depth:depth -> tank:Tank.t -> t -> t
val descend : < descent_speed : speed ; .. > -> end_depth:depth -> tank:Tank.t -> t -> t

val square :
  < descent_speed : speed ; .. > ->
  tank:Tank.t ->
  depth:depth ->
  runtime:time_span ->
  (t, [> `Descent_exceeds_runtime ]) result
(** A square profile, composed of a descent segment to the specified depth and a flat
    bottom segment. [runtime] is the total time of the profile. Returns an error if the
    required descent time is greater than the total time.

    See also {!square_exn}. *)

val square_exn :
  < descent_speed : speed ; .. > -> tank:Tank.t -> depth:depth -> runtime:time_span -> t
(** Cf. {!square}, raises in the error case. *)

(** {2 Traversal} *)

(** All these function traverse the profile in chronological order. *)

val fold_segments : t -> init:'accum -> f:('accum -> Segment.t -> 'accum) -> 'accum
val fold_legs : t -> init:'accum -> f:('accum -> Leg.t -> 'accum) -> 'accum
val map_legs : t -> f:(Leg.t -> 'a) -> 'a list

(** {2 Pretty printing} *)

val to_strings : ?display_short:bool -> t -> string list list
(** Returns a list of list of strings, each sublist describing a segment as
    [direction; depth; duration; runtime; gas] *)

val pp : ?display_short:bool -> Format.formatter -> t -> unit
(** If [display_short] is [false], the segments shorter than a minute are not displayed,
    unless they also include a gas change. The runtime will still take them into account
    (and can thus be larger than the sum of the printed segment times). *)
