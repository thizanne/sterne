open Physics.O
open Physics.Quantity

module Point = struct
  type t = {
    depth : depth;
    time : time_span;
    tank : Tank.t;
  }
  [@@deriving fields]

  let zero ~tank = { depth = 0.; time = Time_ns.Span.zero; tank }
  let create = Fields.create
  let gas point = Tank.gas (tank point)
end

type point = Point.t
type t = point list

let append x y = x @ y

let fold_segments ~f ~init profile =
  let _final_depth, result =
    List.fold_left
      ~f:(fun (initial_depth, acc) { Point.depth; time; tank } ->
        let res = f ~initial_depth ~final_depth:depth ~time ~tank acc in
        (depth, res))
      ~init:(0., init)
      profile
  in
  result

let fold_points ~f ~init profile = List.fold_left profile ~f ~init
let final_depth profile = Point.depth (List.last_exn profile)
let final_tank profile = Point.tank (List.last_exn profile)

let runtime profile =
  fold_points
    profile
    ~f:(fun acc { Point.time; _ } -> Time_ns.Span.(acc + time))
    ~init:Time_ns.Span.zero

let zero ~tank = [ Point.zero ~tank ]

let descend param profile ~depth ~tank =
  (* Positive descent speed, m/min *)
  let current_depth = final_depth profile in
  let time = Time_ns.Span.of_min ((depth - current_depth) / param#descent_speed) in
  profile @ [ Point.create ~depth ~time ~tank ]

let ascend param profile ~depth ~tank =
  (* Positive ascent speed, m/min *)
  let current_depth = final_depth profile in
  let time = Time_ns.Span.of_min ((current_depth - depth) / param#ascent_speed) in
  profile @ [ Point.create ~depth ~time ~tank ]

let stay_for profile ~time ~tank =
  let depth = final_depth profile in
  profile @ [ Point.create ~depth ~time ~tank ]

let stay_until profile ~runtime:final_runtime ~tank =
  let depth = final_depth profile in
  let current_runtime = runtime profile in
  let time = Time_ns.Span.(final_runtime - current_runtime) in
  if Time_ns.Span.is_negative time then failwith "Negative time span"
  else profile @ [ Point.create ~depth ~time ~tank ]

let square param ~tank ~depth ~runtime =
  zero ~tank |> descend param ~depth ~tank |> stay_until ~runtime ~tank

let point_infos
    (previous_depth, previous_runtime, previous_gas)
    ~display_short
    { Point.depth; time; tank } =
  let direction =
    if depth < previous_depth then "↗" else if depth > previous_depth then "↘" else "-"
  in
  let gas = Tank.gas tank in
  let gas_is_new =
    match previous_gas with
    | None -> true
    | Some previous_gas -> Gas.(gas <> previous_gas)
  in
  let box =
    [
      direction;
      Fmt.to_to_string Physics.pp_depth depth;
      Fmt.to_to_string Physics.pp_minutes time;
      Fmt.to_to_string Physics.pp_minutes Time_ns.Span.(previous_runtime + time);
      (if gas_is_new then Fmt.to_to_string Gas.pp gas else "");
    ]
  in
  if display_short || gas_is_new || Time_ns.Span.(time > minute) then Some box else None

let to_strings ?(display_short = false) (profile : t) =
  List.filter_opt
  @@ List.folding_map
       ~f:(fun (depth, runtime, gas) point ->
         ( (point.depth, Time_ns.Span.(runtime + point.time), Some (Point.gas point)),
           point_infos ~display_short (depth, runtime, gas) point ))
       ~init:(0., Time_ns.Span.zero, None)
       profile

let pp ?(display_short = false) ppf profile =
  let infos = to_strings ~display_short profile in
  let box_l = List.map ~f:(List.map ~f:PrintBox.text) infos in
  let first_line_style =
    match Fmt.style_renderer ppf with
    | `None -> PrintBox.Style.default
    | `Ansi_tty -> PrintBox.Style.bold
  in
  let first_line = [ ""; "Depth"; "Stop"; "Runtime"; "Gas" ] in
  let first_line = List.map ~f:(PrintBox.text_with_style first_line_style) first_line in
  let box_l = first_line :: box_l in
  let box = PrintBox.grid_l ~pad:(PrintBox.hpad 1) ~bars:false box_l in
  PrintBox_text.pp ppf box
