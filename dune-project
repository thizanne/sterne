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

(package
 (name sterne-app)
 (synopsis "A dive planning mobile application")
 (depends
  (cordova (>= ~~))
  (cordova-plugin-dialogs (>= ~~))
  (core_kernel (>= ~~))
  (gen_js_api (>= ~~))
  (incr_dom (>= ~~))
  (js_of_ocaml (>= ~~))
  (ppx_jane (>= ~~))
  (sterne (>= ~~))
  ))
