open Float.O

module Quantity = struct
  type fraction = float [@@deriving sexp]
  type percentage = float [@@deriving sexp]
  type percentage_int = int [@@deriving sexp]
  type depth = float [@@deriving sexp]
  type pressure = float [@@deriving sexp]
  type tension = pressure [@@deriving sexp]
  type time_span = Time.Span.t [@@deriving sexp]
  type volume = float [@@deriving sexp]
  type dimensionless = float [@@deriving sexp]
  type other = float [@@deriving sexp]
end

let percent_to_fraction percentage =
  percentage / 100.

let fraction_to_percent frac =
  frac * 100.

let round_fraction_percent_up fraction =
  fraction
  |> fraction_to_percent
  |> Float.round_up
  |> percent_to_fraction

let round_fraction_percent_down fraction =
  fraction
  |> fraction_to_percent
  |> Float.round_down
  |> percent_to_fraction

let litre x =
  x / 1000.

let atmospheric_pressure = 1. (* 1.013 *)

let water_density = 1. (* 1.03 *)

let bar_per_meter =
  water_density / 10.

let depth_to_pressure depth =
  atmospheric_pressure + (depth * bar_per_meter)

let pressure_to_depth pressure =
  (pressure - atmospheric_pressure) / bar_per_meter

let next_3m_depth depth =
  let idepth = Float.iround_down_exn depth in
  let stop = float Int.(idepth - idepth mod 3) in
  (* If depth = 30., should return 27. *)
  if stop = depth then stop - 3. else stop

let pp_time_span ppf time_span =
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
