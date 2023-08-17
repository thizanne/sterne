type t =
  < ascent_speed : Physics.Quantity.other
  ; descent_speed : Physics.Quantity.other
  ; ppo2_max_deco : Physics.Quantity.pressure
  ; ppo2_max_bottom : Physics.Quantity.pressure
  ; sac : Gas_consumption.sac >

let default =
  object
    method ascent_speed = 10.
    method descent_speed = 20.
    method ppo2_max_deco = 1.6
    method ppo2_max_bottom = 1.4
    method sac = Gas_consumption.liters_per_minute 20.
  end
