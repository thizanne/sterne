open Float.O
open Physics.Quantity

(********************)
(* Model definition *)
(********************)

type tension = pressure

type compartment_values = {
  half_life : time_span;
  a : tension (* origin m-value *);
  b : float (* inverse slope *);
}

type compartment = {
  n2 : compartment_values;
  he : compartment_values;
}

type saturation = {
  t_n2 : tension;
  t_he : tension;
}

(* A saturation state is a saturation list for each compartment *)

let alveolar_pressure element mix ambient_pressure =
  let water_vapour_pressure = 0.0627 (* 47 mmHg *) in
  Gas.fraction element mix * (ambient_pressure - water_vapour_pressure)

let compartments =
  let n2_values_table = [
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
  let he_values_table = [
    (* 1 *) (* 1.51, 1.7424, 0.4245; *)
    (* 1b *) 1.88, 1.6189, 0.4770;
    (* 2 *) 3.02, 1.3830, 0.5747;
    (* 3 *) 4.72, 1.1919, 0.6527;
    (* 4 *) 6.99, 1.0458, 0.7223;
    (* 5 *) 10.21, 0.9220, 0.7582;
    (* 6 *) 14.48, 0.8205, 0.7957;
    (* 7 *) 20.53, 0.7305, 0.8279;
    (* 8 *) 29.11, 0.6502, 0.8553;
    (* 9 *) 41.20, 0.5950, 0.8757;
    (* 10 *) 55.19, 0.5545, 0.8903;
    (* 11 *) 70.69, 0.5333, 0.8997;
    (* 12 *) 90.34, 0.5189, 0.9073;
    (* 13 *) 115.29, 0.5181, 0.9122;
    (* 14 *) 147.42, 0.5176, 0.9171;
    (* 15 *) 188.24, 0.5172, 0.9217;
    (* 16 *) 240.03, 0.5119, 0.9267;
  ] in
  let build_values (half_life, a, b) =
    { half_life = Time.Span.of_min half_life; a; b } in
  List.map2_exn
    ~f:(fun n2_values he_values -> {
          n2 = build_values n2_values ;
          he = build_values he_values
        })
    n2_values_table
    he_values_table

(**************************)
(* Saturation computation *)
(**************************)

let full_saturation gas ambient_pressure =
  let t_n2 = alveolar_pressure Gas.N2 gas ambient_pressure in
  let t_he = alveolar_pressure Gas.He gas ambient_pressure in
  List.map
    ~f:(const { t_he; t_n2 })
    compartments

let segment_element_loading element (segment : Dive.Segment.t) p0 { half_life; _ } =
  (* Schreiner equation *)
  (* p0 = initial inert gas pressure in the compartment *)
  let module S = Dive.Segment in
  let initial_pressure = Physics.depth_to_pressure @@ S.initial_depth segment in
  let final_pressure = Physics.depth_to_pressure @@ S.final_depth segment in
  let t = Time.Span.to_min @@ S.duration segment in
  let gas = S.gas segment in
  let half_life = Time.Span.to_min half_life in
  let pressure_variation_rate = (final_pressure - initial_pressure) / t in
  let r = pressure_variation_rate * Gas.fraction element gas in
  let pi_0 = alveolar_pressure element gas initial_pressure in
  let k = log 2. / half_life in
  pi_0 + r * (t - 1. / k) - (pi_0 - p0 - (r / k)) * exp (- k * t)

let segment_compartment_loading segment { t_n2; t_he } { n2; he } =
  {
    t_n2 = segment_element_loading Gas.N2 segment t_n2 n2;
    t_he = segment_element_loading Gas.He segment t_he he;
  }

let segment_saturation saturation segment =
  List.map2_exn
    ~f:(segment_compartment_loading segment)
    saturation
    compartments

let profile_saturation profile =
  Dive.Profile.fold
    ~f:segment_saturation
    ~init:(full_saturation Gas.air Physics.atmospheric_pressure)
    profile

(***************************)
(* Decompression procedure *)
(***************************)

let make_gf_fun (low, high) low_depth =
  (* Returns a staged computation of gradient factors from depth,
     given high and low gf and the depth where gf_low is
     applicable. Gradient factors are expected in the [[0. - 1.]]
     range. *)
  (* Check that 0 < gf < 10 to prevent (80, 80) mistakes and still
     allow 110% gf *)
    Staged.stage @@ fun depth ->
    (depth / low_depth) * (low - high) + high

let get_m_value (a, b) ambient_pressure =
  (* Maximal admissible tension from Bühlmann coefficients *)
  (ambient_pressure / b) + a

let mean_coeff (coeff_n2, t_n2) (coeff_he, t_he) =
  (* Computes the mean of a coefficient, according to its pure n2 and
     pure h2 versions, weighted by the corresponding gas tension *)
  (coeff_n2 * t_n2 + coeff_he * t_he) / (t_n2 + t_he)

let is_admissible_depth_for_compartment gf depth compartment saturation =
  (* Bühlmann coefficients computed as weighted means *)
  let a =
    mean_coeff
      (compartment.n2.a, saturation.t_n2)
      (compartment.he.a, saturation.t_he) in
  let b =
    mean_coeff
      (compartment.n2.b, saturation.t_n2)
      (compartment.he.b, saturation.t_he) in
  (* Maximum admissible total tension *)
  let ambient_pressure =
    Physics.depth_to_pressure depth in
  let m_value =
    get_m_value (a, b) ambient_pressure in
  let m_value_with_gf =
    ambient_pressure + gf depth * (m_value - ambient_pressure) in
  (* Total saturation must be lower than m-value *)
  saturation.t_n2 + saturation.t_he <= m_value_with_gf

let is_admissible_depth gf depth saturation =
  List.for_all2_exn
    ~f:(is_admissible_depth_for_compartment gf depth)
    compartments
    saturation

let first_stop param (gf_low, gf_high) tank depth saturation =
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
        Dive.Segment.ascent_deco param ~tank ~initial_depth:depth ~final_depth:next_3m in
      let next_saturation =
        segment_saturation saturation ascent_segment in
      if is_admissible_depth gf next_3m next_saturation
      then aux low_depth_found gf next_3m next_saturation
      else if low_depth_found
      then depth, saturation, gf
      else
        let gf = Staged.unstage @@ make_gf_fun (gf_low, gf_high) depth in
        aux true gf depth saturation
  in aux false (const gf_low) depth saturation

let rec stop_time gf tank stop_depth next_stop_depth saturation =
  (* Returns the time to be spent at a stop depth, given an initial
     saturation, along with the final saturation after completing this
     stop. This is defined as the time needed to make the saturation
     acceptable at the next stop. Note: the ascent segment to the next
     stop is not considered to compute this saturation. *)
  if is_admissible_depth gf next_stop_depth saturation
  then Time.Span.zero, saturation
  else
    let minute_segment =
      Dive.Segment.minute_deco_stop ~tank ~depth:stop_depth in
    let one_minute =
      Dive.Segment.duration minute_segment in
    let minute_saturation =
      segment_saturation saturation minute_segment in
    let remaining_time, saturation =
      stop_time gf tank stop_depth next_stop_depth minute_saturation in
    Time.Span.(one_minute + remaining_time), saturation

let deco param (gf_low, gf_high) tanks depth bottom_tank saturation =
  (* Returns the full deco profile from given current depth,
     saturation and breathing gas, and available tanks, as a list of
     segments whose last element has 0 as a final depth. *)
  let first_stop_depth, first_stop_saturation, gf =
    first_stop param (gf_low, gf_high) bottom_tank depth saturation in
  let rec deco_stops stop_depth saturation =
    if stop_depth = 0.
    then []
    else
      let next_stop_depth =
        Physics.next_3m_depth stop_depth in
      let deco_tank =
        Tank.find_best_deco param ~depth:stop_depth tanks in
      let waiting_time, end_saturation =
        stop_time gf deco_tank stop_depth next_stop_depth saturation in
      let stop_segment =
        Dive.Segment.flat_deco
          ~tank:deco_tank
          ~depth:stop_depth
          ~duration:waiting_time in
      let ascent_segment =
        Dive.Segment.ascent_deco
          param
          ~tank:deco_tank
          ~initial_depth:stop_depth
          ~final_depth:next_stop_depth
      in
      let next_stop_saturation =
        segment_saturation end_saturation ascent_segment in
      stop_segment ::
      ascent_segment ::
      deco_stops next_stop_depth next_stop_saturation
  in
  Dive.Segment.ascent_deco
    param
    ~tank:bottom_tank
    ~initial_depth:depth
    ~final_depth:first_stop_depth ::
  deco_stops first_stop_depth first_stop_saturation

let deco_procedure param (gf_low, gf_high) dive =
  deco
    param
    (gf_low, gf_high)
    (Dive.tanks dive)
    (Dive.final_depth dive)
    (Dive.final_tank dive)
    (profile_saturation @@ Dive.profile dive)
  |> Dive.Profile.of_segment_list
