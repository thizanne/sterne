open Sterne

let tanks =
  let open Gas in
  let open Tank in
  [al80 @@ tx 20 30; al80 @@ nx 50; al80 @@ oxy]

let profile =
  Dive.square_profile
    (Gas.tx 20 30)
    ~depth:55.
    ~time:(Time.Span.of_min 25.)

let togo =
  Dive.{
    tanks;
    profile;
  }

let deco =
  Buhlmann.deco_procedure (0.3, 0.8) togo

let full =
  Dive.append_profile togo.profile deco

let () =
  Dive.pp_profile Fmt.stdout full
