open Float.O

type t = {
  o2 : float;
  he : float;
  n2 : float;
}

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

let nitrox ~o2 =
  trimix ~he:0. ~o2

let nx o2 =
  nitrox ~o2:(float o2)

let air =
  nx 21

let oxy =
  nx 100

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
