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
  cmdliner
  core
  dune
  fmt
  printbox
  tyre
  uucp
  uutf
  ppx_jane
  ))

(package
 (name sterne-app)
 (synopsis "Scuba dive planning app")
 (depends
  sterne
  core
  cordova
  cordova-plugin-dialogs
  gen_js_api
  incr_dom
  js_of_ocaml
  ))
