module Id = Unique_id.Int63 ()

type t = {
  gas : Gas.t;
  start_pressure : Physics.pressure;
  volume : Physics.volume;
  id : Id.t;
}

let find_best ~ppo2_max ~depth available_tanks =
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

let find_best_deco =
  find_best ~ppo2_max:Param.ppo2_max_deco

let find_best_bottom =
  find_best ~ppo2_max:Param.ppo2_max_bottom

let al80 gas =
  { gas; start_pressure = 207.; volume = 11.1; id = Id.create (); }
