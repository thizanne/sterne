open Float.O

module Quantity = struct
  type fraction = float [@@deriving sexp, compare, equal]
  type percentage = float [@@deriving sexp, compare, equal]
  type percentage_int = int [@@deriving sexp, compare, equal]
  type depth = float [@@deriving sexp, compare, equal]
  type pressure = float [@@deriving sexp, compare, equal]
  type tension = pressure [@@deriving sexp, compare, equal]
  type time_span = Time_float.Span.t [@@deriving sexp, compare, equal]
  type volume = float [@@deriving sexp, compare, equal]
  type normal_volume = float [@@deriving sexp, compare, equal]
  type dimensionless = float [@@deriving sexp, compare, equal]
  type other = float [@@deriving sexp, compare, equal]
end

let percent_to_fraction percentage = percentage / 100.
let fraction_to_percent frac = frac * 100.

let round_fraction_percent_up fraction =
  fraction |> fraction_to_percent |> Float.round_up |> percent_to_fraction

let round_fraction_percent_down fraction =
  fraction |> fraction_to_percent |> Float.round_down |> percent_to_fraction

let litre x = x / 1000.
let to_litre x = x * 1000.
let atmospheric_pressure = 1.013
let water_density = 1.03

let standard_gravity =
  (* https://en.wikipedia.org/wiki/Standard_gravity *)
  9.80665

let bar_per_meter =
  (* ρ.g.h [in bar] = 1000.d.g.h / 100_000 *)
  water_density * standard_gravity / 100.

let depth_to_pressure depth = atmospheric_pressure + (depth * bar_per_meter)
let pressure_to_depth pressure = (pressure - atmospheric_pressure) / bar_per_meter

let next_3m_depth depth =
  let idepth = Float.iround_down_exn depth in
  let stop = float Int.(idepth - (idepth mod 3)) in
  (* If depth = 30., should return 27. *)
  if stop = depth then stop - 3. else stop

let normal_volume_of_gas ~pressure ~volume =
  (* Currently assume ideal gas law *)
  pressure * volume

let pressure_of_gas ~volume ~normal_volume = normal_volume / volume
let volume_of_gas ~normal_volume ~pressure = normal_volume / pressure

let pp_time_span ppf time_span =
  Fmt.string ppf
  @@
  if Time_float.Span.(time_span <. minute) then
    Time_float.Span.to_string_hum time_span ~decimals:0 ~unit_of_time:Unit_of_time.Second
  else
    Time_float.Span.to_string_hum time_span ~decimals:0 ~unit_of_time:Unit_of_time.Minute
    ^ "in" (* 3min rather than 3m *)
