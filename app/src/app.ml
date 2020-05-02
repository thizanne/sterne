open! Incr_dom

module Model = struct
  type t = int [@@deriving sexp,compare]

  let cutoff x y =
    x = y

  let init () =
    0

  let incr x =
    x + 1

  let reset _ =
    0
end

module Action = struct
  type t =
    | Incr_counter
    | Reset_counter
  [@@deriving sexp]

  let should_log _ = true
end

module State = struct
  type t = unit
end

let apply_action model action _ ~schedule_action:_ = match (action : Action.t) with
  | Incr_counter -> Model.incr model
  | Reset_counter -> Model.reset model

let on_startup ~schedule_action:_ _ =
  Async_kernel.return ()

let view m ~inject =
  let open Incr.Let_syntax in
  let open Vdom in
  let button text action =
    Node.button
      [
        Attr.id (String.lowercase text);
        Attr.on_click (fun _ev -> inject action);
        Attr.classes ["mdc-button"; "mdc-button--raised"];
      ]
      [
        Node.div [Attr.class_ "mdc-button__ripple"] [];
        Node.span [Attr.class_ "mdc-button__label"] [Node.text text];
      ]
  in
  let make_counter_element counter =
    Node.h4
      [Attr.class_ "mdc-typography--headline4"]
      [Node.text (string_of_int counter)]
  in
  let incr_button = button "Increment" Action.Incr_counter in
  let reset_button = button "Reset" Action.Reset_counter in
  let%map counter_elt = m >>| make_counter_element in
  Node.div [] [counter_elt; incr_button; reset_button]

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  let%map apply_action =
    model >>| apply_action
  and view = view model ~inject
  and model = model in
  Component.create ~apply_action model view
