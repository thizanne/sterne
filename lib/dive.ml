open Float.O

type segment = {
  initial : Physics.depth;
  final : Physics.depth;
  duration : Physics.time;
  gas : Gas.t;
}

type profile = segment list

let final_depth profile =
  (List.last_exn profile).final

let final_gas profile =
  (List.last_exn profile).gas

type t = {
  profile : profile;
  tanks : Gas.Tank.t list;
}

let ascent_segment gas initial final =
  (* Positive ascent speed *)
  let ascent_speed = 10. in
  let duration = (initial - final) / ascent_speed in
  { gas; initial; final; duration }

let descent_segment gas initial final =
  (* Positive descent speed *)
  let ascent_speed = 20. in
  let duration = round ((final - initial) / ascent_speed) in
  { gas; initial; final; duration }

let horizontal_segment gas ~depth ~duration =
  { gas; initial = depth; final = depth; duration }

let minute_stop_segment gas depth =
  horizontal_segment gas ~depth ~duration:1.

let square_profile gas ~depth ~time =
  let descent =
    descent_segment gas 0. depth in
  let bottom =
    horizontal_segment gas ~depth ~duration:(time - descent.duration) in
  [descent; bottom]
