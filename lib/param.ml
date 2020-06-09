type t = {
  ascent_speed : Physics.Quantity.other;
  descent_speed : Physics.Quantity.other;
  ppo2_max_deco : Physics.Quantity.pressure;
  ppo2_max_bottom : Physics.Quantity.pressure;
} [@@deriving fields]

let default = {
   ascent_speed = 10.;
   descent_speed = 20.;
   ppo2_max_deco = 1.6;
   ppo2_max_bottom = 1.4;
 }
