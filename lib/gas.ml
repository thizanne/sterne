open Float.O

type t = {
  o2 : float;
  he : float;
  n2 : float;
}

module O = struct

  let ( = ) gas gas' =
    let open Int.O in
    Float.robustly_compare gas.o2 gas'.o2 = 0 &&
    Float.robustly_compare gas.n2 gas'.n2 = 0 &&
    Float.robustly_compare gas.he gas'.he = 0

end

let ppo2_deco = 1.6
let ppo2_bottom = 1.4

let fraction element gas = match element with
  | `O2 -> gas.o2
  | `N2 -> gas.n2
  | `He -> gas.he

let from_percent percentage =
  percentage / 100.

let to_percent frac =
  frac * 100.

let trimix ~o2 ~he =
  {
    o2 = from_percent o2;
    he = from_percent he;
    n2 = from_percent (100. - o2 - he)
  }

let tx o2 he =
  trimix ~o2:(float o2) ~he:(float he)

let tx' (o2, he) =
  tx o2 he

let nitrox ~o2 =
  trimix ~he:0. ~o2

let nx o2 =
  nitrox ~o2:(float o2)

let air =
  nx 21

let oxy =
  nx 100

let is_nitrox { he; _ } =
  he = 0.

let is_air gas =
  O.(gas = air)

let is_oxy gas =
  O.(gas = oxy)

let heliair ~o2 =
  let o2 = from_percent o2 in
  let n2 = (o2 / air.o2) * air.n2 in
  let he = 1. - (o2 + n2) in
  { o2; he; n2 }

let mod_ ?(ppo2=1.6) gas =
  Physics.pressure_to_depth (ppo2 / gas.o2)

let end_ gas depth =
  (* Oxygen is conservatively assumed to be as narcotic as nitrogen *)
  Physics.pressure_to_depth @@
  (1. - gas.he) * Physics.depth_to_pressure depth

let ead gas depth =
  assert (is_nitrox gas);
  Physics.pressure_to_depth @@
  gas.n2 * Physics.depth_to_pressure depth / air.n2

let frac element gas =
  match element with
  | `O2 -> gas.o2
  | `He -> gas.he
  | `N2 -> gas.n2

let partial_pressure element gas depth =
  frac element gas * Physics.depth_to_pressure depth

let is_breathable ~ppo2_max ~depth gas =
  partial_pressure `O2 gas depth <= ppo2_max

let pp ppf gas =
  let open Fmt in
  if is_oxy gas
  then string ppf "Oxy"
  else if is_air gas
  then string ppf "Air"
  else if is_nitrox gas
  then pf ppf "Nx %.0f" (to_percent gas.o2)
  else pf ppf "Tx %.0f/%.0f" (to_percent gas.o2) (to_percent gas.he)

module Tank = struct
  module Id = Unique_id.Int63 ()

  type nonrec t = {
    gas : t;
    pressure : Physics.pressure;
    volume : Physics.volume;
    id : Id.t;
  }

  let find_best ~ppo2_max ~depth available_tanks =
    let compare_tank_gasses { gas = gas1; _ } { gas = gas2; _ } =
      (* Negative => tank1 is better. We use lists to
         lexicographically compare gasses: more oxygen is better, then
         more helium is better. *)
      Int.neg @@
      List.compare Float.compare [gas1.o2; gas1.he] [gas2.o2; gas2.he]
    in
    List.find_exn
      ~f:(fun tank -> is_breathable ~ppo2_max ~depth tank.gas)
      (List.sort ~compare:compare_tank_gasses available_tanks)

  let find_best_deco =
    find_best ~ppo2_max:ppo2_deco

  let find_best_bottom =
    find_best ~ppo2_max:ppo2_bottom

  let al80 gas =
    { gas; pressure = 207.; volume = 11.1; id = Id.create (); }
end

module Arg = struct

  let parse input =
    let routes = Tyre.[
        str "air" --> const air;
        str "oxy" --> const oxy;
        pos_int --> nx;
        ((pos_int <* char '/') <&> pos_int) --> tx';
      ] in
    let routes =
      (* Regexps must match the whole string: "air32" isn't a gas, and
         this also ensures that "18/40" is not parsed as nitrox 18 *)
      List.map
        ~f:Tyre.(fun (Route (re, fn)) -> Route (whole_string re, fn))
        routes
    in
    match Tyre.(exec @@ route routes) input with
    | Ok gas -> Some gas
    | Error _ -> None

  let parser =
    Cmdliner.Arg.parser_of_kind_of_string
      ~kind:"a gas (eg. air, oxy, 32 or 10/70)"
      parse

  let conv =
    Cmdliner.Arg.conv ~docv:"GAS" (parser, pp)

end
