open Float.O

type fraction = float

type percentage = float

type percentage_int = int

type depth = float

type pressure = float

type time_span = Time.Span.t

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

let pp_time_span ppf time_span =
  (* Pretty prints a time span. Round to the second for spans lower
     than a minute, to the minute otherwise. Minutes are printed as
     "min" and not "m". *)
  Fmt.string ppf @@
    if Time.Span.(time_span <. minute)
    then
      Time.Span.to_string_hum
        time_span
        ~decimals:0
        ~unit_of_time:Unit_of_time.Second
    else
      Time.Span.to_string_hum
        time_span
        ~decimals:0
        ~unit_of_time:Unit_of_time.Minute ^
      "in" (* 3min rather than 3m *)
