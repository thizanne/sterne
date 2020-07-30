open Float.O
open Physics.Quantity

module Segment = struct
  type t = {
    initial_depth : depth;
    final_depth : depth;
    duration : time_span;
    gas : Gas.t;
    is_deco : bool;
  } [@@deriving fields]

  let is_deco { is_deco; _ } =
    is_deco

  let is_bottom x =
    not (is_deco x)

  let is_deco_transition segment =
    is_deco segment &&
    segment.final_depth <> 0. &&
    segment.initial_depth > segment.final_depth

  let is_deco_stop segment =
    is_deco segment &&
    segment.final_depth = segment.initial_depth

  let ascent (param : Param.t) ~is_deco ~gas ~initial_depth ~final_depth =
    (* Positive ascent speed, m/min *)
    let duration =
      Time.Span.of_min @@
      (initial_depth - final_depth) / param.ascent_speed in
    { gas; initial_depth; final_depth; duration; is_deco }

  let ascent_deco param ~gas ~initial_depth ~final_depth =
    ascent param ~is_deco:true ~gas ~initial_depth ~final_depth

  let ascent_bottom param ~gas ~initial_depth ~final_depth =
    ascent param ~is_deco:false ~gas ~initial_depth ~final_depth

  let is_ascending { initial_depth; final_depth; _ } =
    initial_depth > final_depth

  let descent (param : Param.t) ~gas ~initial_depth ~final_depth =
    (* Positive descent speed, m/min *)
    let duration =
      Time.Span.of_min @@
      (final_depth - initial_depth) / param.descent_speed in
    { gas; initial_depth; final_depth; duration; is_deco = false }

  let is_descending { initial_depth; final_depth; _ } =
    initial_depth < final_depth

  let flat ~gas ~is_deco ~depth ~duration =
    { gas; initial_depth = depth; final_depth = depth; duration; is_deco }

  let flat_deco =
    flat ~is_deco:true

  let flat_bottom =
    flat ~is_deco:false

  let is_flat { initial_depth; final_depth; _ } =
    initial_depth = final_depth

  let minute_deco_stop ~gas ~depth =
    flat_deco ~gas ~depth ~duration:Time.Span.minute
end

module Profile = struct
  type t = Segment.t list

  let one_segment segment =
    [segment]

  let of_segment_list li =
    li

  let add_segment profile segment =
    profile @ [segment]

  let append x y =
    x @ y

  let fold ~f ~init profile =
    List.fold_left ~f ~init profile

  let final_depth profile =
    Segment.final_depth (List.last_exn profile)

  let final_gas profile =
    Segment.gas (List.last_exn profile)

  let square (param : Param.t) ~gas ~depth ~time =
    let descent =
      Segment.descent param ~gas ~initial_depth:0. ~final_depth:depth in
    let bottom =
      Segment.flat_bottom ~gas ~depth ~duration:Time.Span.(time - descent.duration) in
    [descent; bottom]

  let segment_box ~display_transitions ~must_pp_gas start_time segment =
    let open PrintBox in
    let direction =
      if Segment.is_ascending segment
      then "↗"
      else if Segment.is_descending segment
      then "↘"
      else "-" in
    let box =
      [|
        text direction;
        asprintf "%gm" segment.final_depth;
        asprintf "%a" Physics.pp_time_span segment.duration;
        asprintf "%a" Physics.pp_time_span Time.Span.(start_time + segment.duration);
        if must_pp_gas
        then asprintf "%a" Gas.pp segment.gas
        else text "";
      |] in
    if not display_transitions &&
       not must_pp_gas &&
       Segment.is_deco_transition segment
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
              (Time.Span.(run_time + Segment.duration segment), Segment.gas segment),
              segment_box
                ~must_pp_gas:(not Gas.(segment.gas = previous_gas))
                ~display_transitions
                run_time
                segment)
          ~init:(Segment.duration initial_segment, Segment.gas initial_segment)
          segments in
      let box =
        segment_box ~display_transitions ~must_pp_gas:true Time.Span.zero initial_segment ::
        tail_box_lines in
      let box = Option.value_map ~default:box ~f:(Fn.flip List.cons box) first_line in
      PrintBox.(grid ~pad:(hpad 1) ~bars:false @@ Array.of_list box)

  let pp ?(display_transitions=false) ppf profile =
    let () = PrintBox_unicode.setup () in
    let first_line =
      Array.map
        ~f:PrintBox.(text_with_style (Style.bold))
        [|""; "Depth"; "Duration"; "Runtime"; "Gas"|] in
    PrintBox_text.pp ppf (profile_box ~display_transitions ~first_line profile)

end

type t = {
  profile : Profile.t;
  tanks : Tank.t list;
} [@@deriving fields]

let create ~profile ~tanks =
  Fields.create ~profile ~tanks

let final_depth { profile; _ } =
  Profile.final_depth profile

let final_gas { profile; _ } =
  Profile.final_gas profile
