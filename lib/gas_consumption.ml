open Physics.O

type sac = float (* Represented in m^3/sec *)

let liters_per_minute sac =
  let cubic_meters_per_minute = sac / 1000. in
  let cubic_meters_per_second = cubic_meters_per_minute / 60. in
  cubic_meters_per_second

let breathe_at_depth sac ambient_pressure time_span =
  let rmv = sac * ambient_pressure in
  let seconds = Time_ns.Span.to_sec time_span in
  rmv * seconds

let breathe_on_segment sac segment =
  let duration = Profile.Segment.duration segment in
  let mean_depth = Profile.Segment.mean_depth segment in
  let mean_pressure = Physics.depth_to_pressure mean_depth in
  breathe_at_depth sac mean_pressure duration
