open Float.O

type depth = float

type pressure = float

type time = float

let depth_to_pressure d =
  1. + (d / 10.)

let pressure_to_depth p =
  10. * p - 10.
