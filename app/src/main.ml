open! Core_kernel
open! Incr_dom
open! Js_of_ocaml

let on_device_ready () =
  Start_app.start
    (module App)
    ~bind_to_element_with_id:"app"
    ~initial_model:(App.Model.init ())

let () =
  Cordova.Event.device_ready on_device_ready
