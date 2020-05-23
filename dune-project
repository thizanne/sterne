(lang dune 2.5)

(name sterne)
(version %%VERSION%%)
(generate_opam_files true)

(source (github thizanne/sterne))
(authors "Thibault Suzanne")

(package
 (name sterne)
 (synopsis "Scuba dive planning software")
 (depends
  (cmdliner (>= ~~))
  (core_kernel (>= ~~))
  (dune (>= ~~))
  (fmt (>= ~~))
  (printbox (>= ~~))
  (tyre (>= ~~))
  (uucp (>= ~~))
  (uutf (>= ~~))
  (ppx_jane (>= ~~))
  ))
