open Cmdliner

let info =
  let doc = "plan a square dive" in
  let man = [
    `S Manpage.s_bugs;
    `P "Most probably. Use at your own risk. Do not rely on this \
        program alone.";
  ] in
  Term.info "sterne" ~version:"0.1.0-alpha" ~doc ~exits:Term.default_exits ~man

let gases =
  let parser =
    Cmdliner.Arg.parser_of_kind_of_string
      ~kind:"a gas (eg. air, oxy, 32 or 10/70)"
      Gas.parse in
  let gas_conv =
    Cmdliner.Arg.conv ~docv:"GAS" (parser, Gas.pp) in
  let doc = "Available gases. First one will be used as bottom gas." in
  Arg.(value & opt (list gas_conv) [Gas.air] & info ["g"; "gases"] ~doc ~docv:"GASES")

let tanks =
  let create_tank idx gas =
    if idx = 0
    then Tank.double_al80 gas ()
    else Tank.al80 gas () in
  Term.(pure (List.mapi ~f:create_tank) $ gases)

let gf =
  let doc = "Bühlmann gradient factors." in
  Arg.(value & opt (pair ~sep:'/' int int) (80, 80) & info ["gf"] ~doc ~docv:"GF")

let gf =
  let of_percent x = float_of_int x /. 100. in
  Term.(pure (fun (low, high) -> of_percent low, of_percent high) $ gf)

let display_transitions =
  let doc = "Display deco transitions." in
  Arg.(value & flag & info ["display-transitions"] ~doc)

let depth =
  let doc = "Depth." in
  Arg.(required & pos 0 (some float) None & info [] ~docv:"DEPTH" ~doc)

let time =
  let doc = "Dive time." in
  Arg.(required & pos 1 (some float) None & info [] ~docv:"TIME" ~doc)

let time =
  Term.(pure Time.Span.of_min $ time)

let pf_tank_result gas_supply formatter tank =
  let remaining_pressure = Gas_supply.remaining_pressure tank gas_supply in
  let remaining_normal_volume = Gas_supply.remaining_normal_volume tank gas_supply in
  Fmt.pf formatter "%.0f bar (%.0f L) in %a L tank of %a"
    remaining_pressure
    (Physics.to_litre remaining_normal_volume)
    (Fmt.float_dfrac 1) (Physics.to_litre @@ Tank.volume tank)
    Gas.pp (Tank.gas tank)

let main tanks gf display_transitions depth time =
  let param = Param.default in
  let profile = Profile.square param ~tank:(List.hd_exn tanks) ~depth ~time in
  let deco = Buhlmann.deco_procedure param gf tanks profile in
  let full_profile = Profile.append profile deco in
  let gas_supply = Gas_supply.breathe_on_profile' param full_profile in
  Fmt.set_style_renderer Fmt.stdout `Ansi_tty;
  let open Fmt in
  vbox
    (
      const (box (Profile.pp ~display_transitions)) full_profile ++
      cut ++
      cut ++
      styled `Bold (const Fmt.string "Remaining gas") ++
      cut ++
      const (list @@ pf_tank_result gas_supply) tanks ++
      cut
    )
    stdout ();
  ()

let () =
  Term.exit @@
  Term.eval (Term.(pure main $ tanks $ gf $ display_transitions $ depth $ time), info)
