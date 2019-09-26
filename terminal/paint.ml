open Notty
open Notty.Infix
open Idle.S

let simple_border ~style ~char i =
  let horiz_border = I.char style char (I.width i + 2) 1 in
  let vert_border = I.char style char 1 (I.height i) in
  horiz_border <-> (vert_border <|> i <|> vert_border) <-> horiz_border

let border s i =
  (* let a = A.(bg black ++ fg (gray 8)) in *)
  let a = A.empty in
  let single_line_border i =
    let rep c = I.uchar a (Uchar.of_int c)  in
    let horiz_border : image = rep 0x2500 (I.width i) 1 in
    let vert_border : image = rep 0x2502 1 @@ I.height i in
    let top = rep 0x250c 1 1 <|> horiz_border <|> rep 0x2510 1 1 in
    let bottom = rep 0x2514 1 1 <|> horiz_border <|> rep 0x2518 1 1 in
    top <-> (vert_border <|> i <|> vert_border) <-> bottom
  in
  let pipe_border i =
    let horiz_border = I.char a '-' (I.width i) 1 in
    let vert_border = I.char a '|' 1 (I.height i) in
    let corner = I.char a '+' 1 1 in
    (corner <|> horiz_border <|> corner) <->
    (vert_border <|> i <|> vert_border) <->
    (corner <|> horiz_border <|> corner)
  in
  let padded_i = simple_border ~style:A.empty ~char:' ' i in
  let hash_border = simple_border ~style:A.empty ~char:'#' in
  if s.quality.amount > 100. then single_line_border padded_i
  else if s.quality.amount > 10. then pipe_border padded_i
  else hash_border padded_i

let maybe_draw s = function
  | None -> I.void 0 0
  | Some image ->
    border s image

let pp_item = Fmt.strf "%.4G %s"

let stuff s =
  let render_item ((i : item), name) =
    if i.visible then I.string A.(bg black ++ fg white) (pp_item i.amount name) else I.empty in
  let render_change ((i : item), _name) =
    if i.visible then I.string A.(bg black ++ fg yellow) (Idle.Actions.pp_change i.change) else I.empty in
  let render_downstream (i : item) name =
    if i.visible then I.string A.(bg lightyellow ++ fg black ++ st bold) (pp_item i.amount name) else I.empty in
  let out i = render_item i <|> I.void 1 0 <|> render_change i in
  out (s.code, "code") <->
  out (s.quality, "quality") <->
  out (s.hype, "hype") <->
  out (s.camels, "camels") <->
  out (s.docs, "docs") <->
  out (s.reviewers, "reviewers") <->
  (* TODO: should we rename this "users"? *)
  render_downstream s.downstream "downstream" (* don't show change metrics here, since they're confusing *)


let halp s active = I.string A.(bg (gray 1) ++ fg white ++ st bold)
    (Fmt.strf "%s: %s" active.name (active.message s))

let explain active = I.vcat @@ List.map (I.string A.empty) active.explanation

let banner s = I.string A.(bg lightblue ++ fg white)
    (Fmt.strf "your rank: %s" @@ Idle.State.rank s)

let controlpanel_style = A.(fg white ++ st bold)
let inactive_unusable = A.(bg lightred ++ controlpanel_style)
let active_unusable = A.(bg red ++ controlpanel_style)
let inactive_usable = A.(bg lightgreen ++ controlpanel_style)
let active_usable = A.(bg green ++ controlpanel_style)

(* notty doesn't always render the background color of a weird unicode glyph
   wide enough - often there's background there for only half of the visible character.
   hack around this by putting the string representation on top of a background
   with width 2. *)
let background_hack style i =
  i </> Notty.I.char style ' ' 2 1

let control_panel s ~emoji controls = List.fold_left (fun l (n : Board.node) ->
    let active = controls.Board.active_control.control in
    let is_active = (Idle.Actions.compare active n.control) = 0 in
    let c = n.control in
    let label = if emoji then c.emoji else c.name in
    let background =
      match c.is_visible s, c.is_usable s with
      | true, true when is_active -> Some active_usable
      | true, true -> Some inactive_usable
      | true, false when is_active -> Some active_unusable
      | true, false -> Some inactive_unusable
      | false, _ -> None
    in
    match background with
    | None -> l
    | Some style -> l <-> background_hack style (I.string style label)
  ) I.empty controls.board
(* 
let timer ~emoji s =
  let clock = if emoji then "🕰️" else "ticks" in
  I.string A.empty (Fmt.strf "%d %s" s.ticks clock)
*)

let maybe_show_graphs s (graphs : Graphs.t) =
  if s.quality.amount > 0. then
  (maybe_draw s @@ graphs.code <|> maybe_draw s @@ graphs.quality <|> maybe_draw s @@ graphs.hype)
  <->
  (maybe_draw s @@ graphs.camels <|> maybe_draw s @@ graphs.docs <|> maybe_draw s @@ graphs.downstream)
  else I.void 0 0

let render s ~emoji (graphs : Graphs.t) (controls : Board.controls) =
  let b = border s in
  let active = controls.Board.active_control.control in
  (b (banner s) (* <|> b (timer ~emoji s) *)) <->
  (b (stuff s) <|> b (control_panel s ~emoji controls) <|> b (explain active))
  <->
  halp s active
  <->
  maybe_show_graphs s graphs
  <->
  (maybe_draw s @@ graphs.activity)

(* TODO: it would be nice to have a "display this ascii art" function for use with notty *)
(* how does Notty.I.string do on something with newlines? *)
  (* bad. it crashes. *)
let splash_screen term loaded =
  let key_highlight = A.(bg lightblue ++ fg white) in
  let instructions = I.hcat [
      I.string A.empty "select an action with the ";
      I.string key_highlight "arrow keys";
      I.string A.empty " (or ";
      I.string key_highlight "hjkl";
      I.string A.empty "),"
    ] <->
    I.hcat [
      I.string A.empty "and choose it with ";
      I.string key_highlight "Enter";
      I.string A.empty " or ";
      I.string key_highlight "Space"
    ]
  in
  let caveat = I.hcat [
    I.string A.empty "you can do actions highlighted in ";
    I.string inactive_usable "green";
    I.string A.empty ", but not actions highlighted in ";
    I.string inactive_unusable "red";
  ]
  in
  let quit = I.hcat [
      I.string A.empty "Press ";
      I.string key_highlight "Q";
      I.string A.empty " or ";
      I.string key_highlight "Escape";
      I.string A.empty " at any time to save and quit."
    ]
  in
  let i = match loaded with
    | Error `Deserializing -> "couldn't load your game, sorry :( i'll start a new one"
    | Error `No_save -> "starting a brand new game!!"
    | Ok _ -> "loaded your game :)"
  in
  let bold = A.(st bold) in
  Notty_lwt.Term.image term (I.vcat [(I.string bold i);
                                     Art.splash;
                                     instructions;
                                     caveat;
                                     quit;
                                     I.string bold "press any key to continue"])
