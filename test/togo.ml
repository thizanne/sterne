open Sterne

let param = Param.default

let tanks =
  let open Gas in
  let open Tank in
  [al80 @@ tx 20 30; al80 @@ nx 50; al80 @@ oxy]

let profile =
  Dive.Profile.square
    param
    ~gas:(Gas.tx 20 30)
    ~depth:55.
    ~time:(Time.Span.of_min 25.)

let togo =
  Dive.create ~tanks ~profile

let deco =
  Buhlmann.deco_procedure param (0.3, 0.8) togo

let full =
  Dive.Profile.append (Dive.profile togo) deco

let () =
  Dive.Profile.pp Fmt.stdout full
