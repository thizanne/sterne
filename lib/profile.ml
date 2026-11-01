open Physics.O
open Physics.Quantity

module Step = struct
  (** A low-level, bare representation of the minimal data needed to represent a dive
      profile as a sequence of steps. Steps do not make sense on their own. In particular,
      they do not carry initial depth information. *)

  type t = {
    end_depth : depth;
    duration : time_span;
    tank : Tank.t;
  }
  [@@deriving fields]

  let create = Fields.create

  let ascent param ~start_depth ~end_depth ~tank =
    (* Positive ascent speed, m/min *)
    if end_depth > start_depth then failwith "ascent";
    let duration = Time_ns.Span.of_min ((start_depth - end_depth) / param#ascent_speed) in
    { end_depth; duration; tank }

  let descent param ~start_depth ~end_depth ~tank =
    (* Positive descent speed, m/min *)
    if end_depth < start_depth then failwith "descent";
    let duration =
      Time_ns.Span.of_min ((end_depth - start_depth) / param#descent_speed)
    in
    { end_depth; duration; tank }
end

module Segment = struct
  (** Steps with a start depth. Segments can make sense on their own and therefore export
      creation functions. In particular, they contain the needed data for decompression
      computations while still not carrying anchored information that only makes sense
      within a dive profile. *)

  type t = {
    start_depth : depth;
    (* The following are copied from steps. An alternative would be to define a single
       [step] field, but this would still require O(n) lifting functions to conveniently
       access its nested fields. *)
    end_depth : depth;
    duration : time_span;
    tank : Tank.t;
  }
  [@@deriving fields]

  type direction =
    | Ascent
    | Descent
    | Level

  let create = Fields.create

  let of_step ~start_depth { Step.end_depth; duration; tank } =
    { start_depth; end_depth; duration; tank }

  let gas segment = Tank.gas (tank segment)

  let direction segment =
    if end_depth segment < start_depth segment then Ascent
    else if end_depth segment > start_depth segment then Descent
    else Level

  let average_depth { start_depth; end_depth; _ } = (start_depth +. end_depth) /. 2.

  let stay_for ~depth ~duration ~tank =
    { start_depth = depth; end_depth = depth; duration; tank }

  let minute_stop ~depth ~tank = stay_for ~depth ~duration:Time_ns.Span.minute ~tank

  let ascent param ~start_depth ~end_depth ~tank =
    of_step ~start_depth (Step.ascent param ~start_depth ~end_depth ~tank)
end

module Leg = struct
  (** Rich, anchored information that dive profile consumers can use for various
      processing purpose. A leg does not make sense on its own but only within the dive
      profile it belongs to, hence no creation function is public. *)

  type t = {
    segment : Segment.t;
    start_runtime : time_span;
    previous_tank : Tank.t option;
  }
  [@@deriving fields]

  type direction = Segment.direction =
    | Ascent
    | Descent
    | Level

  let duration leg = Segment.duration (segment leg)
  let start_depth leg = Segment.start_depth (segment leg)
  let end_depth leg = Segment.end_depth (segment leg)
  let average_depth leg = Segment.average_depth (segment leg)
  let tank leg = Segment.tank (segment leg)
  let gas leg = Tank.gas (tank leg)
  let end_runtime t = Time_ns.Span.(start_runtime t + duration t)
  let direction leg = Segment.direction (segment leg)

  let is_new_tank leg =
    match previous_tank leg with
    | None -> true
    | Some previous_tank -> Tank.(previous_tank <> tank leg)
end

type 'a nonempty_list = 'a list
(** Lists with an implicit non-empty invariant. *)

type t = {
  steps : Step.t nonempty_list;
      (** The steps are stored in reversed chronological order (ie. the list has a stack
          semantics), to allow efficient growth from the end.

          Invariant: the list is never empty. *)
  runtime : time_span;  (** Runtime is cached and equal to the sum of steps durations. *)
}
[@@deriving fields]

(** Accessors *)

let last_step { steps; _ } =
  match steps with
  | [] -> failwith "Broken invariant: empty profile"
  | final_step :: _ -> final_step

let last_depth profile = Step.end_depth (last_step profile)
let last_tank profile = Step.tank (last_step profile)

(** Creation *)

let initial_descent param ~end_depth ~tank =
  let step = Step.descent param ~start_depth:0. ~end_depth ~tank in
  { steps = [ step ]; runtime = Step.duration step }

let add_step step { steps; runtime } =
  { steps = step :: steps; runtime = Time_ns.Span.(runtime + Step.duration step) }

let descend param ~end_depth ~tank profile =
  (* Positive descent speed, m/min *)
  let start_depth = last_depth profile in
  add_step (Step.descent param ~start_depth ~end_depth ~tank) profile

let ascend param ~end_depth ~tank profile =
  (* Positive ascent speed, m/min *)
  let start_depth = last_depth profile in
  add_step (Step.ascent param ~start_depth ~end_depth ~tank) profile

let stay_for ~duration ~tank profile =
  let end_depth = last_depth profile in
  add_step (Step.create ~end_depth ~duration ~tank) profile

let minute_stop ~tank profile = stay_for ~duration:Time_ns.Span.minute ~tank profile

let stay_until ~runtime:runtime_ ~tank profile =
  let end_depth = last_depth profile in
  let current_runtime = runtime profile in
  let duration = Time_ns.Span.(runtime_ - current_runtime) in
  if Time_ns.Span.(duration <= zero) then Error `Descent_exceeds_runtime
  else Ok (add_step { Step.end_depth; duration; tank } profile)

let square param ~tank ~depth ~runtime =
  initial_descent param ~end_depth:depth ~tank |> stay_until ~runtime ~tank

let square_exn param ~tank ~depth ~runtime =
  square param ~tank ~depth ~runtime |> Error.ok_or_raise

(** Traversal *)

let fold_steps { steps; _ } ~init ~f =
  (* Implementation note: [fold_right] would work as well but its Core implementation is
     equivalent to this with extra closure allocations. *)
  List.fold_left (List.rev steps) ~f ~init

let fold_segments profile ~init ~f =
  let _last_depth, result =
    fold_steps profile ~init:(0., init) ~f:(fun (start_depth, acc) step ->
        let segment = Segment.of_step ~start_depth step in
        let acc' = f acc segment in
        (Segment.end_depth segment, acc'))
  in
  result

let fold_legs profile ~init ~f =
  let _final_runtime, _last_tank, result =
    fold_segments
      profile
      ~init:(Time_ns.Span.zero, None, init)
      ~f:(fun (runtime, current_tank, acc) segment ->
        let leg =
          Leg.Fields.create ~start_runtime:runtime ~previous_tank:current_tank ~segment
        in
        let acc' = f acc leg in
        let runtime' = Time_ns.Span.(runtime + Leg.duration leg) in
        let current_tank' = Some (Leg.tank leg) in
        (runtime', current_tank', acc'))
  in
  result

let map_legs profile ~f =
  List.rev @@ fold_legs profile ~init:[] ~f:(fun rev_result leg -> f leg :: rev_result)

(** Pretty-printing *)

let leg_box_texts ~display_short leg =
  let direction =
    match Leg.direction leg with Ascent -> "↗" | Descent -> "↘" | Level -> "-"
  in
  let tank_is_new = Leg.is_new_tank leg in
  let gas_text = if tank_is_new then Fmt.to_to_string Gas.pp (Leg.gas leg) else "" in
  let box_texts =
    [
      direction;
      Fmt.to_to_string Physics.pp_depth (Leg.end_depth leg);
      Fmt.to_to_string Physics.pp_minutes (Leg.duration leg);
      Fmt.to_to_string Physics.pp_minutes (Leg.end_runtime leg);
      gas_text;
    ]
  in
  if display_short || tank_is_new || Time_ns.Span.(Leg.duration leg >= minute) then
    Some box_texts
  else None

let box_rows ?(display_short = false) (profile : t) =
  List.filter_opt @@ map_legs profile ~f:(fun leg -> leg_box_texts ~display_short leg)

let to_strings = box_rows

let pp ?(display_short = false) ppf profile =
  let leg_rows = box_rows ~display_short profile in
  let leg_rows = List.map ~f:(List.map ~f:PrintBox.text) leg_rows in
  let first_row_style =
    match Fmt.style_renderer ppf with
    | `None -> PrintBox.Style.default
    | `Ansi_tty -> PrintBox.Style.bold
  in
  let first_row = [ ""; "Depth"; "Stop"; "Runtime"; "Gas" ] in
  let first_row = List.map ~f:(PrintBox.text_with_style first_row_style) first_row in
  let grid = first_row :: leg_rows in
  let box = PrintBox.grid_l ~pad:(PrintBox.hpad 1) ~bars:false grid in
  PrintBox_text.pp ppf box
