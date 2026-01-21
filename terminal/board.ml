open Idle.S
module Actions = Idle.Actions

type node = {
  control : control;
  left : node option;
  right : node option;
  up : node option;
  down : node option;
}

type controls = {
  active_control : node;
  board : node list;
}

let rec lambda_node =
  { control = Actions.plus_lambda;
    left = None; right = None; up = None; down = Some sparkle_node;}
and sparkle_node =
  { control = Actions.sparkle;
    left = None; right = None; up = Some lambda_node; down = Some beta_node;}
and beta_node =
  {control = Actions.beta_reduce;
   left = None; right = None; up = Some sparkle_node; down = Some package_node;}
and package_node =
  {control = Actions.release;
   left = None; right = None; up = Some beta_node; down = Some camel_node;}
and camel_node =
  {control = Actions.contributors;
   left = None; right = None; up = Some package_node; down = Some docs_node;}
and docs_node =
  {control = Actions.docs;
   left = None; right = None; up = Some camel_node; down = Some reviewer_node;}
and reviewer_node =
  {control = Actions.reviewers;
   left = None; right = None; up = Some docs_node; down = None}

let nodes = [lambda_node; sparkle_node; beta_node; package_node; camel_node; docs_node; reviewer_node;]

let start_controls = {
  active_control = List.hd nodes;
  board = nodes;
}

let check_vis s = function
  | None -> None
  | Some node ->
    if node.control.is_visible s then Some node else None

let update_selected s controls e =
  let step n = function
    | `Right -> n.right
    | `Left -> n.left
    | `Up -> n.up
    | `Down -> n.down
  in
  let new_control =
    match e with
    | `Key (`Arrow dir) -> step controls.active_control dir |> check_vis s
    | `Key `Home -> Some lambda_node (* always available! *)
    | _ -> None
  in
  match new_control with
  | None -> s, controls
  | Some n ->
    s, {controls with active_control = n; }

let update s c event =
  let draw_board = update_selected s c in
  match event with
  | `Key ((`Escape | `ASCII 'q'), _mods) -> { s with lets_bail = true }, c
  | `Key (`Arrow _ as e, _mods) -> draw_board (`Key e)
      (* allow use of vi-keys as well *)
  | `Key ((`ASCII 'h'), _mods) -> draw_board (`Key (`Arrow `Left))
  | `Key ((`ASCII 'j'), _mods) -> draw_board (`Key (`Arrow `Down))
  | `Key ((`ASCII 'k'), _mods) -> draw_board (`Key (`Arrow `Up))
  | `Key ((`ASCII 'l'), _mods) -> draw_board (`Key (`Arrow `Right))
  | `Key (`Enter, _mods) when c.active_control.control.is_usable s ->
    c.active_control.control.action s, c
  | `Key ((`ASCII ' '), _mods) when c.active_control.control.is_usable s ->
    c.active_control.control.action s, c
  | _ -> s, c
