open Physics.Quantity

(** {1 Diving tanks } *)

type t [@@deriving sexp]

module O : sig
  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

include (module type of O)

val gas : t -> Gas.t
val start_pressure : t -> pressure
val volume : t -> volume
val normal_volume_full : t -> normal_volume

val create : gas:Gas.t -> start_pressure:pressure -> volume:volume -> unit -> t
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

val al80 : Gas.t -> t
