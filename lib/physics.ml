open Float.O

type depth = float

type pressure = float

type time = float

type volume = float

let atmospheric_pressure = 1. (* 1.013 *)

let water_density = 1. (* 1.03 *)

let bar_per_meter =
  water_density / 10.

let depth_to_pressure depth =
  atmospheric_pressure + (depth * bar_per_meter)

let pressure_to_depth pressure =
  (pressure - atmospheric_pressure) / bar_per_meter

let next_3m_depth depth =
  (* The deeper depth that is both strictly shallower than the [depth]
     parameter and a multiple of 3m *)
  let idepth = Float.iround_down_exn depth in
  let stop = float Int.(idepth - idepth mod 3) in
  (* If depth = 30., should return 27. *)
  if stop = depth then stop - 3. else stop
