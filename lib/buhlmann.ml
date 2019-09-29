open Float.O

(********************)
(* Model definition *)
(********************)

type tension = Physics.pressure

type compartment = {
  half_life : Physics.time;
  m_value : Physics.pressure -> tension;
}

type saturation = tension list

let alveolar_pressure element mix ambient_pressure =
  let water_vapour_pressure = 0.0627 (* 47 mmHg *) in
  Gas.fraction element mix * (ambient_pressure - water_vapour_pressure)

let m_value_from_coeff ~a ~b p =
  (* Maximal admissible tension *)
  (p /. b) +. a

let n2_compartments =
  let table = [
    (* 1 *) (* 4.0, 1.2599, 0.5240; *)
    (* 1b *) 5.0, 1.1696, 0.5578;
    (* 2 *) 8.0, 1.0000, 0.6514;
    (* 3 *) 12.5, 0.8618, 0.7222;
    (* 4 *) 18.5, 0.7562, 0.7825;
    (* 5 *) 27.0, 0.6491, 0.8126;
    (* 6 *) 38.3, 0.5316, 0.8434;
    (* 7 *) 54.3, 0.4681, 0.8693;
    (* 8 *) 77.0, 0.4301, 0.8910;
    (* 9 *) 109., 0.4049, 0.9092;
    (* 10 *) 146., 0.3719, 0.9222;
    (* 11 *) 187., 0.3447, 0.9319;
    (* 12 *) 239., 0.3176, 0.9403;
    (* 13 *) 305., 0.2828, 0.9477;
    (* 14 *) 390., 0.2716, 0.9544;
    (* 15 *) 498., 0.2523, 0.9602;
    (* 16 *) 635., 0.2327, 0.9653;
  ] in
  List.map table ~f:(
    fun (half_life, a, b) -> {
        half_life;
        m_value = m_value_from_coeff ~a ~b
      }
  )

(**************************)
(* Saturation computation *)
(**************************)

let full_saturation ambient_pressure =
  List.map
    ~f:(const @@ alveolar_pressure `N2 Gas.air ambient_pressure)
    n2_compartments

let segment_compartment_loading element (segment : Dive.segment) p0 compartment =
  (* Schreiner equation *)
  let initial_pressure = Physics.depth_to_pressure segment.initial in
  let final_pressure = Physics.depth_to_pressure segment.final in
  let t = segment.duration in
  let pressure_variation_rate = (final_pressure - initial_pressure) / t in
  let r = pressure_variation_rate * Gas.fraction element segment.gas in
  let pi_0 = alveolar_pressure element segment.gas initial_pressure in
  let k = log 2. / compartment.half_life in
  pi_0 + r * (t - 1. / k) - (pi_0 - p0 - (r / k)) * exp (- k * t)

let segment_saturation element saturation segment =
  List.map2_exn
    ~f:(segment_compartment_loading element segment)
    saturation
    n2_compartments

let profile_saturation element profile =
  List.fold_left
    ~f:(segment_saturation element)
    ~init:(full_saturation Physics.atmospheric_pressure)
    profile

(***************************)
(* Decompression procedure *)
(***************************)

module Gf = struct
  type t = float

  let gf_fun (low, high) low_depth =
    (* Returns a staged computation of gradient factors from depth,
       given high and low gf and the depth where gf_low is
       applicable. Gradient factors are expected in the [[0. - 1.]]
       range. *)
    (* Check that 0 < gf < 10 to prevent (80, 80) mistakes and still
       allow 110% gf *)
    Staged.stage @@ fun depth ->
    (depth / low_depth) * (low - high) + high

  let m_value gf depth compartment =
    (* Returns the modified M-value according to gradient factor [gf]
       for the given [depth] and [compartment] arguments. *)
    let ambient_pressure = Physics.depth_to_pressure depth in
    let standard_mvalue = compartment.m_value ambient_pressure in
    ambient_pressure + gf depth * (standard_mvalue - ambient_pressure)
end

let is_admissible_depth gf depth saturation =
  List.for_all2_exn
    ~f:(fun c tension -> Gf.m_value gf depth c >= tension)
    n2_compartments
    saturation

let first_stop (gf_low, gf_high) depth saturation =
  (* Given a depth and a saturation, returns the depth of the first
     required deco stop, along with the saturation when reaching it
     and the corresponding gradient factor function. This stop is
     defined as the shallower 3m-multiple that can be reached with an
     acceptable saturation. Ascent to the stop is taken into account
     to compute this saturation. *)
  let rec aux low_depth_found gf depth saturation =
    (* If the low depth for gf hasn't been determined yet, then we
       only consider gf_low for acceptable saturation. Once we reach
       the M-value with this gf_low, then we can compute a gf function
       for the following ascent. Note that it can happen that when the
       gf_low-m_value has been reached, one can actually still ascend
       to a shallower first stop because the actual gradient factor
       will start increasing, thus relaxing the m-value
       restriction. *)
    if depth = 0.
    then 0., saturation, gf
    else
      let next_3m =
        Physics.next_3m_depth depth in
      let ascent_segment =
        Dive.ascent_segment Gas.air depth next_3m in
      let next_saturation =
        segment_saturation `N2 saturation ascent_segment in
      if is_admissible_depth gf next_3m next_saturation
      then aux low_depth_found gf next_3m next_saturation
      else if low_depth_found
      then depth, saturation, gf
      else
        let gf = Staged.unstage @@ Gf.gf_fun (gf_low, gf_high) depth in
        aux true gf depth saturation
  in aux false (const gf_low) depth saturation

let rec stop_time gf gas stop_depth next_stop_depth saturation =
  (* Returns the time to be spent at a stop depth, given an initial
     saturation, along with the final saturation after completing this
     stop. This is defined as the time needed to make the saturation
     acceptable at the next stop. Note: the ascent segment to the next
     stop is not considered to compute this saturation. *)
  if is_admissible_depth gf next_stop_depth saturation
  then 0., saturation
  else
    let minute_segment =
      Dive.minute_stop_segment gas stop_depth in
    let minute_saturation =
      segment_saturation `N2 saturation minute_segment in
    let remaining_time, saturation =
      stop_time gf gas stop_depth next_stop_depth minute_saturation in
    minute_segment.duration + remaining_time, saturation

let deco (gf_low, gf_high) tanks depth gas saturation =
  (* Returns the full deco profile from given current depth,
     saturation and breathing gas, and available tanks, as a list of
     segments whose last element has 0 as a final depth. *)
  let first_stop_depth, first_stop_saturation, gf =
    first_stop (gf_low, gf_high) depth saturation in
  let rec deco_stops stop_depth saturation =
    if stop_depth = 0.
    then []
    else
      let next_stop_depth =
        Physics.next_3m_depth stop_depth in
      let gas =
        (Gas.Tank.find_best_deco ~depth:stop_depth tanks).gas in
      let waiting_time, end_saturation =
        stop_time gf gas stop_depth next_stop_depth saturation in
      let stop_segment =
        Dive.horizontal_segment gas ~depth:stop_depth ~duration:waiting_time in
      let ascent_segment =
        Dive.ascent_segment gas stop_depth next_stop_depth in
      let next_stop_saturation =
        segment_saturation `N2 end_saturation ascent_segment in
      stop_segment ::
      ascent_segment ::
      deco_stops next_stop_depth next_stop_saturation
  in
  Dive.ascent_segment gas depth first_stop_depth ::
  deco_stops first_stop_depth first_stop_saturation

let deco_procedure (gf_low, gf_high) { Dive.tanks; profile } =
  deco
    (gf_low, gf_high)
    tanks
    (Dive.final_depth profile)
    (Dive.final_gas profile)
    (profile_saturation `N2 profile)
