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
    ~time:(Time_ns.Span.of_min 25.)

let deco = Buhlmann.deco_procedure param (0.3, 0.8) tanks togo_profile
let full = Profile.append togo_profile deco

let%expect_test "togo" =
  Profile.pp Fmt.stdout full;
  [%expect
    {|
       Depth  Stop    Runtime  Gas
    ↘  55 m   3 min   3 min    20/30
    -  55 m   22 min  25 min
    -  24 m   2 min   30 min
    -  21 m   1 min   31 min   50 %
    -  18 m   1 min   33 min
    -  15 m   2 min   35 min
    -  12 m   2 min   37 min
    -  9 m    5 min   43 min
    -  6 m    6 min   49 min   O2
    -  3 m    10 min  59 min |}]
