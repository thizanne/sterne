(lang dune 2.5)

(name sterne)
(version %%VERSION%%)
(generate_opam_files true)

(source (github thizanne/sterne))
(authors "Thibault Suzanne <thi.suzanne@gmail.com>")
(maintainers "Thibault Suzanne <thi.suzanne@gmail.com>")

(package
 (name sterne)
 (synopsis "Scuba dive planning software")
 (depends
  cmdliner
  core
  dune
  fmt
  printbox-text
  tyre
  uucp
  uutf
  ppx_jane
  (odig :with-dev-setup)
  (utop :with-dev-setup)
  (merlin :with-dev-setup)
  (ocp-indent :with-dev-setup)
  (ocamlformat :with-dev-setup)
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
  (odig :with-dev-setup)
  (utop :with-dev-setup)
  (merlin :with-dev-setup)
  (ocp-indent :with-dev-setup)
  (ocamlformat :with-dev-setup)
  ))
