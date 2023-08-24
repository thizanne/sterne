open Physics

let%expect_test "pp_fraction_as_round_percent" =
  List.iter
    ~f:(fun x -> Fmt.pr "%a@\n" pp_fraction_as_percent x)
    [ 0.; 0.05; 0.1; 0.12345; 0.55; 0.1664; 0.99; 1.; 1.234 ];
  [%expect
    {|
    0 %
    5 %
    10 %
    12.35 %
    55 %
    16.64 %
    99 %
    100 %
    123.4 % |}]

let%expect_test "pp_time_span" =
  List.iter
    ~f:(fun x -> Fmt.pr "%a@\n" pp_minutes x)
    Time_ns.Span.
      [
        of_sec 0.;
        of_sec 1.;
        of_sec 1.1;
        of_sec 2.5;
        of_sec 29.;
        of_sec 30.;
        of_min 1.;
        of_min 2.;
        of_min 2.1;
        of_min 2.5;
        of_min 120.;
      ];
  [%expect
    {|
      0 s
      1 s
      1 s
      3 s
      29 s
      1 min
      1 min
      2 min
      2 min
      3 min
      120 min |}]
