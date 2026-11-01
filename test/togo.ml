open Sterne

let param = Param.default

let tanks =
  let open Gas in
  let open Tank in
  [ double_al80 (tx 20 30) (); al80 (nx 50) (); al80 oxy () ]

let togo_profile =
  Profile.square_exn
    param
    ~tank:(List.hd_exn tanks)
    ~depth:55.
    ~runtime:(Time_ns.Span.of_min 25.)

let complete_profile = Buhlmann.plan_deco param (0.3, 0.8) tanks togo_profile

let%expect_test "togo" =
  Profile.pp Fmt.stdout complete_profile;
  [%expect
    {|
       Depth  Stop    Runtime  Gas
    ↘  55 m   3 min   3 min    20/30
    -  55 m   22 min  25 min
    ↗  24 m   3 min   28 min
    -  24 m   2 min   30 min
    -  21 m   1 min   31 min   50 %
    -  18 m   1 min   33 min
    -  15 m   2 min   35 min
    -  12 m   2 min   37 min
    -  9 m    5 min   43 min
    -  6 m    6 min   49 min   O2
    -  3 m    10 min  59 min
    |}]
