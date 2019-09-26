open Idle.S
open Notty
open Notty.Infix

type t = {
  code : Notty.image option;
  quality : Notty.image option;
  hype : Notty.image option;
  camels : Notty.image option;
  docs : Notty.image option;
  downstream : Notty.image option;
  activity : Notty.image option;
}

let nope = {
  code = None;
  quality = None;
  hype = None;
  camels = None;
  docs = None;
  downstream = None;
  activity = None;
}

let rec get_n l n =
  if n < 0 then []
  else match List.nth_opt l n with
    | Some q -> q :: get_n l (n - 1)
    | None -> []

(* also TODO: flip to different scales (log, etc) *)
let graph_amounts ?(last_n=10) title (i : item) =
  match i.visible with
  | false -> None
  | true ->
    let height = 10 in
    let attr = A.(bg black ++ fg green) in
    let n_samples = min (List.length i.last_amounts) last_n in
    (* get_n reverses the list, which is OK since we want to update from the right anyway *)
    let samples = get_n i.last_amounts (n_samples - 1) in
    let _min, max = Idle.State.get_min_max samples in
    let scale = 
      if max <= 0. then 1 (* TODO: need a float module in Bar to do this properly, but we should be able to display real tiny values and remove this special case *)
      else if max <= 10. then 1
      else if max <= 100. then 10
      else if max <= 1000. then 100
      else int_of_float (max /. 5.)
    in Some (
      I.string A.(st underline) title <->
      Bar.Int.vertical ~attr ~height ~scale ~y_min:0 (List.map int_of_float samples) </> I.void (last_n + 2) 0)

let graphs_of_state (s : state) = {
  code = graph_amounts "code" s.code;
  quality = graph_amounts "quality" s.quality;
  hype = graph_amounts "hype" s.hype;
  camels = graph_amounts "camels" s.camels;
  docs = graph_amounts "docs" s.docs;
  downstream = graph_amounts "downstream" s.downstream;
  activity = Some (Banner.banner s);
}

let maybe_update s graphs =
  match s.ticks mod s.graph_interval with
  | 0 -> graphs_of_state s
  | _ -> graphs

