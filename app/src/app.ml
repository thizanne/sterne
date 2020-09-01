open Incr_dom
open Sterne
open Js_of_ocaml

module Model = struct
  type t = {
    depth : Physics.Quantity.depth;
    time : Physics.Quantity.time_span;
  } [@@deriving sexp,compare,equal,fields]

  let cutoff x y =
    equal x y

  let init () = {
    depth = 0.;
    time = Time.Span.zero;
  }

  let update_depth t depth =
    { t with depth }

  let update_time t time =
    { t with time }
end

module Action = struct
  type t =
    | Update_depth of Physics.Quantity.depth
    | Update_time of Physics.Quantity.time_span
  [@@deriving sexp]

  let should_log _ = true
end

module State = struct
  type t = unit
end

let apply_action model action _ ~schedule_action:_ = match (action : Action.t) with
  | Update_depth depth -> Model.update_depth model depth
  | Update_time time -> Model.update_time model time

let on_startup ~schedule_action:_ _ =
  Async_kernel.return ()

let one_segment_node segment =
  let open Vdom in
  Node.tr [Attr.class_ "mdc-data-table__row"] @@
  List.map
    ~f:(fun elem -> Node.td [Attr.class_ "mdc-data-table__cell"] [Node.text elem])
    segment

let runtime_node profile =
  let open Vdom in
  let strings = Profile.to_strings profile in
  Node.div [Attr.class_ "mdc-data-table"] [
    Node.div [Attr.class_ "mdc-data-table__table-container"] [
      Node.table [Attr.class_ "mdc-data-table__table"] [
        Node.tbody [Attr.class_ "mdc-data-table__content"] @@
        List.map ~f:one_segment_node strings
      ]
    ]
  ]

let view m ~inject =
  let open Incr.Let_syntax in
  let open Vdom in
  let input name action =
    let input_id = String.lowercase name in
    let label_id = "label-" ^ input_id in
    Node.label
      [
        Attr.classes ["mdc-text-field"; "mdc-text-field--filled"];
        Attr.create "data-mdc-auto-init" "MDCTextField";
      ]
      [
        Node.span [Attr.class_ "mdc-text-field__ripple"] [];
        Node.input
          [
            Attr.id input_id;
            Attr.class_ "mdc-text-field__input";
            Attr.type_ "text";
            Attr.create "aria-labelledby" label_id;
            Attr.on_input
              (fun _ev text -> inject (action text));
          ] [];
        Node.span
          [Attr.class_ "mdc-floating-label"; Attr.id label_id]
          [Node.text name];
        Node.span [Attr.class_ "mdc-line-ripple"] [];
      ] in
  let depth_input =
    input
      "Depth"
      (fun text -> Action.Update_depth (float_of_string text)) in
  let time_input =
    input
      "Time"
      (fun text -> Action.Update_time (Time.Span.of_min @@ float_of_string text)) in
  let mdc_cell elt =
    Node.div [Attr.class_ "mdc-layout-grid__cell--span-2"] [elt]
  in
  let%map runtime_node =
    m >>| fun { Model.depth; time } ->
    let profile =
      Profile.square
        Param.default
        ~tank:(Tank.al80 Gas.air)
        ~depth ~time in
    let tanks = [Tank.al80 Gas.air] in
    let gf = (0.8, 0.8) in
    let deco = Buhlmann.deco_procedure Param.default gf tanks profile in
    let full_profile = Profile.append profile deco in
    runtime_node full_profile
    in
    Node.div []
    [
      Node.div
        [Attr.class_ "mdc-layout-grid"]
        [
          Node.div
            [Attr.class_ "mdc-layout-grid__inner"]
            [mdc_cell depth_input; mdc_cell time_input];
        ];
      Node.div
        [Attr.create "style" "display: flex; justify-content: center"]
        [runtime_node]
    ]

let on_display _state ~schedule_action:_ =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "mdc.autoInit") [||]

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  let%map apply_action =
    model >>| apply_action
  and view = view model ~inject
  and model = model in
  Component.create ~apply_action ~on_display model view
