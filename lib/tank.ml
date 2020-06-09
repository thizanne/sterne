module Id = Unique_id.Int63 ()

module T = struct
  type t = {
    gas : Gas.t;
    start_pressure : Physics.Quantity.pressure;
    volume : Physics.Quantity.volume;
    id : Id.t;
  } [@@deriving fields, sexp]

  let compare { id = x; _ } { id = y; _ } =
    Id.compare x y
end

module O = Comparable.Make (T)

include T
include O

let create ~gas ~start_pressure ~volume () =
  Fields.create ~id:(Id.create ()) ~gas ~start_pressure ~volume

let find_best ~ppo2_max ~depth available_tanks =
  (* TODO: move the comparison logic in Gas, and consider returning
     the less bad gas if none can be breathed. Also consider the empty
     list. And maybe a balancing option, switching from the current
     tank only if one is found with more pressure. *)
  let compare_tank_gasses { gas = gas1; _ } { gas = gas2; _ } =
    (* Negative => tank1 is better. We use lists to
       lexicographically compare gasses: more oxygen is better, then
       more helium is better. *)
    Int.neg @@
    List.compare
      Float.compare
      [Gas.fraction Gas.O2 gas1; Gas.fraction Gas.He gas1]
      [Gas.fraction Gas.O2 gas2; Gas.fraction Gas.He gas2]
  in
  List.find_exn
    ~f:(fun tank -> Gas.is_breathable ~ppo2_max ~depth tank.gas)
    (List.sort ~compare:compare_tank_gasses available_tanks)

let find_best_deco (param : Param.t) =
  find_best ~ppo2_max:param.ppo2_max_deco

let find_best_bottom (param : Param.t)=
  find_best ~ppo2_max:param.ppo2_max_bottom

let al80 gas =
  { gas; start_pressure = 207.; volume = Physics.litre 11.1; id = Id.create (); }
