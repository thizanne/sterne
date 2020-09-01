open Physics.Quantity

(** A segment is a dive part where one gas is breathed, and the
    vertical speed is constant *)
module Segment : sig
  type t

  val initial_depth : t -> depth
  val final_depth : t -> depth
  val duration : t -> time_span
  val tank : t -> Tank.t
  val gas : t -> Gas.t

  (** Dive segments are either bottom or decompression segments. Deco
      segments are basically all segments from the start of the deco procedure.
  *)

  val is_deco : t -> bool

  val is_deco_transition : t -> bool
  (** A deco transition is a segment that joins two deco stops. The
      first ascent to a deco stop is not a deco transition, except if
      this deco stop is 3 m or less above the bottom, eg. on multilevel
      dives. In that particular case, the result is undefined (but the
      function does return a valid boolean).
  *)

  val is_deco_stop : t -> bool
  val is_bottom : t -> bool

  val is_ascending : t -> bool
  val is_descending : t -> bool
  val is_flat : t -> bool

  val flat_bottom : tank:Tank.t -> depth:depth -> duration:time_span -> t
  val flat_deco : tank:Tank.t -> depth:depth -> duration:time_span -> t

  val minute_deco_stop : tank:Tank.t -> depth:depth -> t
  (** A deco stop segment that longs one minute. *)

  val ascent_deco : Param.t -> tank:Tank.t -> initial_depth:depth -> final_depth:depth -> t
  val ascent_bottom : Param.t -> tank:Tank.t -> initial_depth:depth -> final_depth:depth -> t

  val descent : Param.t -> tank:Tank.t -> initial_depth:depth -> final_depth:depth -> t
  (** Descent segments are necessarily bottom ones. *)
end

type t

val final_depth : t -> depth
val final_tank : t -> Tank.t

val one_segment : Segment.t -> t
(** A profile composed on one single segment. *)

val add_segment : t -> Segment.t -> t
(** Add a segment at the end of a profile. *)

val of_segment_list : Segment.t list -> t

val square : Param.t -> tank:Tank.t -> depth:depth -> time:time_span -> t
(** A square profile, composed of a descent segment to the specified
    depth and a flat bottom segment. [time] is the total time of the
    profile. The behaviour is unspecified if the needed descent time
    is greater than the total time. *)

val append : t -> t -> t
(** Append two profiles by concatenating their segments. *)

val fold : f:('a -> Segment.t -> 'a) -> init:'a -> t -> 'a

(** {2 Pretty printing} *)

val to_strings : ?display_transitions:bool -> t -> string list list
(** Returns a list of list of strings, each sublist describing
    a segment as [direction; depth; duration; runtime; gas] *)

val pp : ?display_transitions:bool -> Format.formatter -> t -> unit
(** If [display_transitions] is [false], the decompression
    transition segments are not displayed. *)
