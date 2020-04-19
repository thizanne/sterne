(lang dune 2.5)

(name sterne)
(version %%VERSION%%)
(generate_opam_files true)

(source (github thizanne/sterne))
(authors "Thibault Suzanne")

(package
 (name sterne)
 (synopsis "A dive planning library")
 (depends
  (cmdliner (>= ~~))
  (core_kernel (>= ~~))
  (dune (>= ~~))
  (fmt (>= ~~))
  (printbox (>= ~~))
  (tyre (>= ~~))
  (uucp (>= ~~))
  (uutf (>= ~~))
  ))

(package
 (name sterne-bin)
 (depends
  (core_kernel (>= ~~))
  (cmdliner (>= ~~))
  (fmt (>= ~~))
  (sterne (>= ~~))
  ))
