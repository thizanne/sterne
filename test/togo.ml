open Sterne

let param = Param.default

let tanks =
  let open Gas in
  let open Tank in
  [ double_al80 (tx 20 30) (); al80 (nx 50) (); al80 oxy () ]

let togo_profile =
  Profile.square
    param
    ~tank:(List.hd_exn tanks)
    ~depth:55.
    ~time:(Time_float.Span.of_min 25.)

let deco = Buhlmann.deco_procedure param (0.3, 0.8) tanks togo_profile
let full = Profile.append togo_profile deco
let () = Profile.pp Fmt.stdout full
