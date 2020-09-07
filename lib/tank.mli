open Physics.Quantity

(** {1 Diving tanks } *)

type t [@@deriving sexp]

module O : sig
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

include (module type of O)

include Comparator.S with type t := t
module Map : Map.S with type Key.t = t
module Set : Set.S with type Elt.t = t

val gas : t -> Gas.t
val volume : t -> volume
val start_pressure : t -> pressure
val normal_volume_full : t -> normal_volume

val create : gas:Gas.t -> volume:volume -> start_pressure:pressure -> unit -> t
(** Create a new tank with specified parameters. Tanks created through
    distinct calls to [create] will be distinct. *)

(** {2 Selecting a tank} *)

val find_best : ppo2_max:pressure -> depth:depth -> t list -> t
(** Returns the best tank on which the diver can breathe at the given
   depth, without exceeding maximal ppo2. If no tank from the list can
   be used at that depth, behaviour is unspecified. *)

val find_best_deco : < ppo2_max_deco : pressure; .. > -> depth:depth -> t list -> t
val find_best_bottom : < ppo2_max_bottom : pressure; .. > -> depth:depth -> t list -> t

(** {2 Common tanks} *)

type tank_creator = Gas.t -> unit -> t
(** A tank creator takes a gas and returns a function that, each time
    it is called, generates a new distinct tank.
*)

val al80 : tank_creator
val double_al80 : tank_creator
val al_7l : tank_creator
