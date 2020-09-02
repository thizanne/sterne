open Float.O

type t = Physics.Quantity.normal_volume Tank.Map.t

let empty = Tank.Map.empty

let full_tanks tanks =
  List.fold
    ~init:empty
    ~f:(fun accum tank -> Map.set accum ~key:tank ~data:(Tank.normal_volume_full tank))
    tanks

let set_tank_full tank supply =
  Map.set supply ~key:tank ~data:(Tank.normal_volume_full tank)

let remaining_normal_volume tank supply =
  Map.find_exn supply tank

let remaining_pressure tank supply =
  let normal_volume = remaining_normal_volume tank supply in
  Physics.pressure_of_gas ~normal_volume ~volume:(Tank.volume tank)

let breathe_on_segment param segment supply =
  let breathed = Gas_consumption.breathe_on_segment param#sac segment in
  let tank = Profile.Segment.tank segment in
  (* If tank is absent, leave supply unchanged *)
  Map.change
    ~f:(fun norm_vol_opt -> match norm_vol_opt with
        | None -> None
        | Some normal_volume -> Some (normal_volume - breathed))
    supply tank

let breathe_on_profile param profile supply =
  Profile.fold
    ~f:(fun accum seg -> breathe_on_segment param seg accum)
    ~init:supply
    profile

let breathe_on_segment' param segment supply =
  let breathed = Gas_consumption.breathe_on_segment param#sac segment in
  let tank = Profile.Segment.tank segment in
  (* If tank is absent, then consider it starts at full volume *)
  Map.update
    ~f:(fun norm_vol_opt -> match norm_vol_opt with
        | None -> Tank.normal_volume_full tank - breathed
        | Some normal_volume -> normal_volume - breathed)
    supply tank

let breathe_on_profile' param profile =
  Profile.fold
    ~f:(fun accum seg -> breathe_on_segment' param seg accum)
    ~init:empty
    profile
