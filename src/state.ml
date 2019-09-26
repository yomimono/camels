open S

let start_item : item = {amount = 0.; change = None; visible = true; last_amounts = []}
let start_process : process = {active = false; visible = false; }

let start_state : state =
{
  lets_bail = false;
  ticks = 0;
  graph_max = 491; (* the maximum number of values to store in the `last_amounts` lists for items. For a nice display in the activity banner, this should be a multiple of seven, plus one.*)
  graph_interval = 10; (* update graphs every this-many ticks *)
  last_tick = 0L;
  code = start_item;
  quality = start_item;
  hype = start_item;
  camels = {start_item with amount = 1.; visible = false};
  reviewers = {start_item with visible = false};
  downstream = {start_item with visible = false};
  docs = {start_item with visible = false};
  ci = {start_process with visible = false};
  cd = {start_process with visible = false};
}

let peg n = if n < 0. then 0. else n

let rec drop_to n l =
  if List.length l <= n then l else drop_to n (List.tl l)

let add ~max l i =
  if List.length l >= max then
    i :: (List.rev (drop_to (max - 1) (List.rev l)))
  else i :: l

let get_min_max l =
  let init = Float.infinity, Float.neg_infinity in
  List.fold_left 
      (fun (prev_min, prev_max) amount ->
         let next_min = if amount < prev_min then amount else prev_min in
         let next_max = if amount > prev_max then amount else prev_max in
         (next_min, next_max)) init l

let run_tick ~ticks ~interval ~max {amount; change; visible; last_amounts } =
  match change with
  | None ->
    let last_amounts =
      if ticks mod interval = 0
      then add ~max last_amounts amount
      else last_amounts
    in
    {amount; change; visible; last_amounts}
  | Some change ->
    let new_amount = (amount +. (Float.pow change.exp_const change.exp) +. change.mult_const *. change.mult +. change.add_const) in
    let new_amount = peg new_amount in
    let last_amounts =
      if ticks mod interval = 0
      then add ~max last_amounts new_amount
      else last_amounts
    in
    { amount = new_amount;
      change = Some change;
      visible;
      last_amounts;
    }

let run_downstream s =
  let demand = s.hype.amount in
  let potential = s.docs.amount in
  let downstream_users = if demand < potential then demand else potential in
  let downstream_users = peg downstream_users in
  let hype_change = 0.0001 *. downstream_users in
  let new_downstream = downstream_users > 0.
                       && s.downstream.amount = 0. in
  let downstream =
    if new_downstream then {s.downstream with amount = downstream_users; visible = true}
    else begin
      let new_users = Random.float (downstream_users -. s.downstream.amount) in
      let amount = peg (s.downstream.amount +. new_users) in
      {s.downstream with amount = amount }
    end
  in
  { s with downstream;
           hype = {s.hype with change = Some {Actions.no_change with add_const = hype_change }};
  }

let add_ci s quality =
  if s.ci.active then {quality with amount = quality.amount +. s.camels.amount *. 0.01}
  else quality

let tick s =
  let s = run_downstream s in
  let ticks = s.ticks + 1 in
  let step = run_tick ~ticks ~interval:s.graph_interval ~max:s.graph_max in
  { s with ticks;
           code = step s.code;
           quality = step s.quality |> add_ci s;
           hype = step s.hype;
           camels = step s.camels;
           docs = step s.docs;
           reviewers = step s.reviewers;
           downstream = step s.downstream;
  }

let rank s =
  let total = s.camels.amount +. s.reviewers.amount +. s.downstream.amount in
  if total <= 1. then "noob"
  else if total <= 10. then "small fry"
  else if total <= 100. then "big fish in a small pond"
  else if total <= 1_000. then "kind of a big deal"
  else if total <= 10_000. then "camel enabler"
  else if total <= 100_000. then "camel embiggener"
  else if total <= 1_000_000. then "camel evangelist"
  else if total <= 10_000_000. then "camel epidemic"
  else if total <= 100_000_000. then "destroyer of imperative programs"
  else if total <= 1_000_000_000. then "memory safeguard"
  else "destroyer of non-camel worlds"
