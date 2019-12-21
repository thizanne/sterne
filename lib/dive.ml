open Float.O

type segment = {
  initial : Physics.depth;
  final : Physics.depth;
  duration : Physics.time_span;
  gas : Gas.t;
  is_deco : bool;
}

let is_deco { is_deco; _ } =
  is_deco

let is_deco_transition segment =
  (* A deco transition is a segment that joins two deco stops. If the
     first deco stop is 3m above the bottom (eg. on multilevel dives),
     then we also consider that the first ascension may be
     a transition. *)
  is_deco segment &&
  segment.final <> 0. &&
  segment.initial - segment.final = 3.

type profile = segment list

let final_depth profile =
  (List.last_exn profile).final

let final_gas profile =
  (List.last_exn profile).gas

let append_profile profile1 profile2 =
  List.append profile1 profile2

type t = {
  profile : profile;
  tanks : Gas.Tank.t list;
}

let ascent_deco_segment gas initial final =
  (* Positive ascent speed, m/min *)
  let ascent_speed = 10. in
  let duration = Time.Span.of_min @@ (initial - final) / ascent_speed in
  { gas; initial; final; duration; is_deco = true }

let is_ascending { initial; final; _ } =
  initial > final

let descent_segment gas initial final =
  (* Positive descent speed, m/min *)
  let descent_speed = 20. in
  let duration = Time.Span.of_min @@ (final - initial) / descent_speed in
  { gas; initial; final; duration; is_deco = false }

let is_descending { initial; final; _ } =
  initial < final

let flat_segment gas ~is_deco ~depth ~duration =
  { gas; initial = depth; final = depth; duration; is_deco }

let flat_deco_segment =
  flat_segment ~is_deco:true

let flat_bottom_segment =
  flat_segment ~is_deco:false

let is_flat { initial; final; _ } =
  initial = final

let minute_stop_segment gas depth =
  flat_deco_segment gas ~depth ~duration:Time.Span.minute

let square_profile gas ~depth ~time =
  let descent =
    descent_segment gas 0. depth in
  let bottom =
    flat_bottom_segment gas ~depth ~duration:Time.Span.(time - descent.duration) in
  [descent; bottom]

let segment_box ~display_transitions ~must_pp_gas start_time segment =
  let open PrintBox in
  let direction =
    if is_ascending segment
    then "↗"
    else if is_descending segment
    then "↘"
    else "-" in
  let box =
    [|
      text direction;
      asprintf "%gm" segment.final;
      asprintf "%a" Physics.pp_time_span segment.duration;
      asprintf "%a" Physics.pp_time_span Time.Span.(start_time + segment.duration);
      if must_pp_gas
      then asprintf "%a" Gas.pp segment.gas
      else text "";
    |] in
  if not display_transitions &&
     not must_pp_gas &&
     is_deco_transition segment
     (* TODO: if we allow gas changing during the ascent and not only
        at a stop, then it may happen within a transition. In that
        case we should print both this transition segment and the
        previous one. Currently we assume it does not happen. However,
        deco transition where we change gas may still happen if the
        first transition happens to be 3m and the higher ppo2 on deco
        allows gas changing. Thus we must still test must_pp_gas.*)
  then Array.map ~f:(Fn.const empty) box
  else box

let profile_box ?first_line ~display_transitions profile =
  match profile with
  | [] -> assert false
  | initial_segment :: segments ->
    let tail_box_lines =
      (* All segment boxes but the first one *)
      List.folding_map
        ~f:(fun (run_time, previous_gas) segment ->
            (Time.Span.(run_time + segment.duration), segment.gas),
            segment_box
              ~must_pp_gas:(not Gas.O.(segment.gas = previous_gas))
              ~display_transitions
              run_time
              segment)
        ~init:(initial_segment.duration, initial_segment.gas)
        segments in
    let box =
      segment_box ~display_transitions ~must_pp_gas:true Time.Span.zero initial_segment ::
      tail_box_lines in
    let box = Option.value_map ~default:box ~f:(Fn.flip List.cons box) first_line in
    PrintBox.(grid ~pad:(hpad 1) ~bars:false @@ Array.of_list box)

let pp_profile ?(display_transitions=false) ppf profile =
  let () = PrintBox_unicode.setup () in
  let first_line =
    Array.map
      ~f:PrintBox.(text_with_style (Style.bold))
      [|""; "Depth"; "Duration"; "Runtime"; "Gas"|] in
  PrintBox_text.pp ppf (profile_box ~display_transitions ~first_line profile)
