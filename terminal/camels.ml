open Idle.S
open Lwt.Infix
open Cmdliner

(* args:
   savefile
   seed
   low-char font mode
   no-graphs mode *)

(* we use `opt string` rather than `opt file` because it's OK if the file
 * doesn't already exist; we still want the user to be able to say where
 * they want the save file for a new game *)
let savefile =
  let doc = "save and load from this file" in
  Arg.(value & opt string "camels.sav" & info ["f"; "file"] ~doc)

let eye_candy =
  let doc = "show graphs and other pretty (but inessential) visualizations.  If the game is slow, disable this" in
  Arg.(value & opt bool true & info ["g"; "graphs"] ~doc)

let emoji =
  let doc = "use emoji" in
  Arg.(value & opt bool true & info ["e"; "emoji"] ~doc)

let seed =
  let doc = "seed for randomization" in
  Arg.(value & opt int 0xc0ffee & info ["s"; "seed"] ~doc)

let tick_duration = 250_000_000L

let try_load_state savefile =
  try (let file = Yojson.Safe.from_file savefile in
       match state_of_yojson file with
       | Error _ -> Error `Deserializing, Idle.State.start_state
       | Ok state -> Ok (), state)
  with
  | _ -> (Error `No_save, Idle.State.start_state)

let save file state =
  Yojson.Safe.(to_file file @@ state_to_yojson {state with lets_bail = false;
                                                           last_tick = 0L})
let quit savefile term state =
  save savefile state;
  Notty_lwt.Term.release term

let tick state controls =
  (Time.sleep_ns tick_duration >|= fun () ->
   (Idle.State.tick {state with last_tick = Mclock.elapsed_ns ()}, controls))

let user_input term state controls =
  Lwt_stream.last_new (Notty_lwt.Term.events term) >>= fun event ->
  let state, controls = Board.update state controls event in
  let now = Mclock.elapsed_ns () in
  if Int64.(sub now state.last_tick) >= tick_duration then
    Lwt.return (Idle.State.tick {state with last_tick = Mclock.elapsed_ns ()}, controls)
  else 
    Lwt.return (state, controls)

let rec loop savefile ~do_graphs ~emoji term state graphs controls : unit Lwt.t =
  let display = Paint.render ~emoji state graphs controls in
  Notty_lwt.Term.image term display >>= fun () ->
  Lwt.pick [
    tick state controls;
    user_input term state controls;
  ] >>= fun (new_state, new_controls) ->
    if new_state.lets_bail
    then quit savefile term new_state
    else begin
      let new_graphs =
        if do_graphs then Graphs.maybe_update state graphs else graphs
      in
      loop savefile ~do_graphs ~emoji term new_state new_graphs new_controls [@tailrec]
    end

let makeitgo savefile seed eye_candy emoji =
  let term = Notty_lwt.Term.create () in
  Random.init seed;
  let loaded, start_state = try_load_state savefile in
  let start_state = {start_state with last_tick = Mclock.elapsed_ns ()} in
  let graphs =
    if eye_candy then Graphs.graphs_of_state start_state else Graphs.nope
  in
  let do_graphs = eye_candy in
  Lwt_main.run @@
  (Paint.splash_screen term loaded >>= fun () ->
  Lwt_stream.last_new (Notty_lwt.Term.events term) >>= fun _event ->
  loop savefile ~do_graphs ~emoji term start_state graphs Board.start_controls)

let cmd = Term.(const makeitgo $ savefile $ seed $ eye_candy $ emoji), Term.info "camels" ~exits:Term.default_exits

let () = Term.(exit @@ eval cmd)
