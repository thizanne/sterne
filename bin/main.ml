open Cmdliner
open Sterne

let info =
  let doc = "plan a square dive" in
  let man = [
    `S Manpage.s_bugs;
    `P "Most probably. Use at your own risk. Do not rely on this \
        program alone.";
  ] in
  Term.info "sterne" ~version:"0.1.0-alpha" ~doc ~exits:Term.default_exits ~man

let gases =
  let doc = "Available gases. First one will be used as bottom gas." in
  Arg.(value & opt (list Gas.Arg.conv) [Gas.air] & info ["g"; "gases"] ~doc ~docv:"GASES")

let gf =
  let doc = "Bühlmann gradient factors." in
  Arg.(value & opt (pair ~sep:'/' int int) (80, 80) & info ["gf"] ~doc ~docv:"GF")

let gf =
  let of_percent x = float_of_int x /. 100. in
  Term.(pure (fun (low, high) -> of_percent low, of_percent high) $ gf)

let tanks =
  Term.(pure (List.map ~f:Gas.Tank.al80) $ gases)

let depth =
  let doc = "Depth." in
  Arg.(required & pos 0 (some float) None & info [] ~docv:"DEPTH" ~doc)

let time =
  let doc = "Dive time." in
  Arg.(required & pos 1 (some float) None & info [] ~docv:"TIME" ~doc)

let time =
  Term.(pure Time.Span.of_min $ time)

let main tanks gf depth time =
  let profile = Dive.square_profile (List.hd_exn tanks).Gas.Tank.gas ~depth ~time in
  let dive = Dive.{ tanks; profile } in
  let deco = Buhlmann.deco_procedure gf dive in
  let full_profile = Dive.append_profile profile deco in
  Fmt.pr "@[%a@]@." Dive.pp_profile full_profile

let () =
  Term.exit @@
  Term.eval (Term.(pure main $ tanks $ gf $ depth $ time), info)
