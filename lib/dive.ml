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

let append_profile profile1 profile2 =
  List.append profile1 profile2

type t = {
  profile : profile;
  tanks : Gas.Tank.t list;
}

let ascent_segment gas initial final =
  (* Positive ascent speed *)
  let ascent_speed = 10. in
  let duration = (initial - final) / ascent_speed in
  { gas; initial; final; duration }

let is_ascending { initial; final; _ } =
  initial > final

let descent_segment gas initial final =
  (* Positive descent speed *)
  let descent_speed = 20. in
  let duration = (final - initial) / descent_speed in
  { gas; initial; final; duration }

let is_descending { initial; final; _ } =
  initial < final

let flat_segment gas ~depth ~duration =
  { gas; initial = depth; final = depth; duration }

let is_flat { initial; final; _ } =
  initial = final

let minute_stop_segment gas depth =
  flat_segment gas ~depth ~duration:1.

let square_profile gas ~depth ~time =
  let descent =
    descent_segment gas 0. depth in
  let bottom =
    flat_segment gas ~depth ~duration:(time - descent.duration) in
  [descent; bottom]

let segment_box ?(must_pp_gas=true) start_time segment =
  let open PrintBox in
  let direction =
    if is_ascending segment
    then "↗"
    else if is_descending segment
    then "↘"
    else "-" in
  [|
    text direction;
    float_ segment.final;
    float_ segment.duration;
    float_ (start_time + segment.duration);
    if must_pp_gas
    then asprintf "%a" Gas.pp segment.gas
    else text "";
  |]

let profile_box ?first_line profile =
  match profile with
  | [] -> assert false
  | initial_segment :: segments ->
    let _acc, tail_box_lines =
      (* All segment boxes but the first one *)
      List.fold_map
        ~f:(fun (run_time, previous_gas) segment ->
            (run_time + segment.duration, segment.gas),
            segment_box
              ~must_pp_gas:(not Gas.O.(segment.gas = previous_gas))
              run_time
              segment)
        ~init:(initial_segment.duration, initial_segment.gas)
        segments in
    let box = segment_box 0. initial_segment :: tail_box_lines in
    let box = Option.value_map ~default:box ~f:(Fn.flip List.cons box) first_line in
    PrintBox.(grid ~pad:(hpad 1) ~bars:false @@ Array.of_list box)

let pp_profile ppf profile =
  let () = PrintBox_unicode.setup () in
  let first_line =
    Array.map
      ~f:PrintBox.(text_with_style (Style.bold))
      [|""; "depth"; "duration"; "runtime"; "gas"|] in
  PrintBox_text.pp ppf (profile_box ~first_line profile)
