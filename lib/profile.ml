open Physics.O
open Physics.Quantity

module Segment = struct
  type t = {
    initial_depth : depth;
    final_depth : depth;
    duration : time_span;
    tank : Tank.t;
    is_deco : bool;
  }
  [@@deriving fields]

  let mean_depth { initial_depth; final_depth; _ } = (initial_depth + final_depth) / 2.
  let gas { tank; _ } = Tank.gas tank
  let is_deco { is_deco; _ } = is_deco
  let is_bottom x = not (is_deco x)

  let is_deco_transition segment =
    is_deco segment
    && segment.final_depth <> 0.
    && segment.initial_depth > segment.final_depth

  let is_deco_stop segment =
    is_deco segment && segment.final_depth = segment.initial_depth

  let ascent param ~is_deco ~tank ~initial_depth ~final_depth =
    (* Positive ascent speed, m/min *)
    let duration =
      Time_float.Span.of_min @@ ((initial_depth - final_depth) / param#ascent_speed)
    in
    { tank; initial_depth; final_depth; duration; is_deco }

  let ascent_deco param ~tank ~initial_depth ~final_depth =
    ascent param ~is_deco:true ~tank ~initial_depth ~final_depth

  let ascent_bottom param ~tank ~initial_depth ~final_depth =
    ascent param ~is_deco:false ~tank ~initial_depth ~final_depth

  let is_ascending { initial_depth; final_depth; _ } = initial_depth > final_depth

  let descent param ~tank ~initial_depth ~final_depth =
    (* Positive descent speed, m/min *)
    let duration =
      Time_float.Span.of_min @@ ((final_depth - initial_depth) / param#descent_speed)
    in
    { tank; initial_depth; final_depth; duration; is_deco = false }

  let is_descending { initial_depth; final_depth; _ } = initial_depth < final_depth

  let flat ~tank ~is_deco ~depth ~duration =
    { tank; initial_depth = depth; final_depth = depth; duration; is_deco }

  let flat_deco = flat ~is_deco:true
  let flat_bottom = flat ~is_deco:false
  let is_flat { initial_depth; final_depth; _ } = initial_depth = final_depth

  let minute_deco_stop ~tank ~depth =
    flat_deco ~tank ~depth ~duration:Time_float.Span.minute
end

type t = Segment.t list

let one_segment segment = [ segment ]
let of_segment_list li = li
let add_segment profile segment = profile @ [ segment ]
let append x y = x @ y
let fold ~f ~init profile = List.fold_left ~f ~init profile
let final_depth profile = Segment.final_depth (List.last_exn profile)
let final_tank profile = Segment.tank (List.last_exn profile)

let square param ~tank ~depth ~time =
  let descent = Segment.descent param ~tank ~initial_depth:0. ~final_depth:depth in
  let bottom =
    Segment.flat_bottom ~tank ~depth ~duration:Time_float.Span.(time - descent.duration)
  in
  [ descent; bottom ]

let segment_infos ~display_transitions ~must_pp_gas start_time segment =
  let direction =
    if Segment.is_ascending segment then "↗"
    else if Segment.is_descending segment then "↘"
    else "-"
  in
  let box =
    [
      direction;
      Fmt.to_to_string Physics.pp_depth segment.final_depth;
      Fmt.to_to_string Physics.pp_time_span segment.duration;
      Fmt.to_to_string
        Physics.pp_time_span
        Time_float.Span.(start_time + segment.duration);
      (if must_pp_gas then Fmt.to_to_string Gas.pp (Segment.gas segment) else "");
    ]
  in
  if
    (not display_transitions) && (not must_pp_gas) && Segment.is_deco_transition segment
    (* TODO: if we allow gas changing during the ascent and not only at a stop, then it
       may happen within a transition. In that case we should print both this transition
       segment and the previous one. Currently we assume it does not happen. However, deco
       transition where we change gas may still happen if the first transition happens to
       be 3m and the higher ppo2 on deco allows gas changing. Thus we must still test
       must_pp_gas.*)
  then None
  else Some box

let to_strings ?(display_transitions = false) profile =
  match profile with
  | [] -> assert false
  | initial_segment :: segments ->
      let tail_box_lines =
        (* All segment boxes but the first one *)
        List.folding_map
          ~f:(fun (run_time, previous_gas) segment ->
            ( (Time_float.Span.(run_time + Segment.duration segment), Segment.gas segment),
              segment_infos
                ~must_pp_gas:(not Gas.(Segment.gas segment = previous_gas))
                ~display_transitions
                run_time
                segment ))
          ~init:(Segment.duration initial_segment, Segment.gas initial_segment)
          segments
      in
      let box =
        segment_infos
          ~display_transitions
          ~must_pp_gas:true
          Time_float.Span.zero
          initial_segment
        :: tail_box_lines
      in
      List.filter_opt box

let pp ?(display_transitions = false) ppf profile =
  let infos = to_strings ~display_transitions profile in
  let box_l = List.map ~f:(List.map ~f:PrintBox.text) infos in
  let first_line = [ ""; "Depth"; "Stop"; "Runtime"; "Gas" ] in
  let bold = PrintBox.text_with_style PrintBox.Style.bold in
  let first_line = List.map ~f:bold first_line in
  let box_l = first_line :: box_l in
  let box = PrintBox.grid_l ~pad:(PrintBox.hpad 1) ~bars:false box_l in
  PrintBox_text.pp ppf box
