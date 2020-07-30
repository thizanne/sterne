open Float.O

type t = {
  o2 : float;
  he : float;
  n2 : float;
} [@@deriving sexp]

type element =
  | O2
  | He
  | N2

let equal gas gas' =
  let open Int.O in
  Float.robustly_compare gas.o2 gas'.o2 = 0 &&
  Float.robustly_compare gas.n2 gas'.n2 = 0 &&
  Float.robustly_compare gas.he gas'.he = 0

let ( = ) gas gas' =
  equal gas gas'

let ( <> ) gas gas' =
  not (gas = gas')

let fraction element gas = match element with
  | O2 -> gas.o2
  | N2 -> gas.n2
  | He -> gas.he

let from_percent percentage =
  percentage / 100.

let to_percent frac =
  frac * 100.

let trimix' ~o2 ~he =
  { o2; he; n2 = 1. - o2 - he }

let trimix ~o2 ~he =
  trimix' ~o2:(from_percent o2) ~he:(from_percent he)

let tx o2 he =
  trimix ~o2:(float o2) ~he:(float he)

let tx' (o2, he) =
  tx o2 he

let nitrox' ~o2 =
  trimix' ~o2 ~he:0.

let nitrox ~o2 =
  trimix ~o2 ~he:0.

let nx o2 =
  nitrox ~o2:(float o2)

let air =
  nx 21

let oxy =
  nx 100

let is_nitrox { he; _ } =
  he =. 0.

let is_air gas =
  gas = air

let is_oxy gas =
  gas = oxy

let heliair ~o2 =
  let o2 = from_percent o2 in
  let n2 = (o2 / air.o2) * air.n2 in
  let he = 1. - (o2 + n2) in
  { o2; he; n2 }

let mod_ ?(ppo2_max=1.6) gas =
  Physics.pressure_to_depth (ppo2_max / gas.o2)

let end_ gas depth =
  (* Oxygen is conservatively assumed to be as narcotic as nitrogen *)
  Physics.pressure_to_depth @@
  (1. - gas.he) * Physics.depth_to_pressure depth

let ead gas depth =
  assert (is_nitrox gas);
  Physics.pressure_to_depth @@
  gas.n2 * Physics.depth_to_pressure depth / air.n2

let partial_pressure element gas depth =
  fraction element gas * Physics.depth_to_pressure depth

let is_breathable ?(ppo2_max=1.6) ~depth gas =
  partial_pressure O2 gas depth <= ppo2_max

let pp ppf gas =
  let open Fmt in
  if is_oxy gas
  then string ppf "Oxy"
  else if is_air gas
  then string ppf "Air"
  else if is_nitrox gas
  then pf ppf "Nx %.0f" (to_percent gas.o2)
  else pf ppf "Tx %.0f/%.0f" (to_percent gas.o2) (to_percent gas.he)

let parse input =
  (* TODO: be case insensitive, and allow mix prefix (Nx, Tx). *)
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
