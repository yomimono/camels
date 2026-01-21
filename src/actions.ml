open S

let always = fun _ -> true

let compare a b =
  compare a.name b.name

let no_change : tick_change = {
  exp_const = 0.;
  exp = 1.;
  mult_const = 1.;
  mult = 0.;
  add_const = 0.;
}
let pp_change = function
  | None -> Fmt.str ""
  | Some c ->
    let exp = match c.exp_const, c.exp with
      | 0., 1. -> None 
      | const, exp -> Some (Fmt.str "%.2G ^ %.2G" const exp)
    in
    let mult = match c.mult_const, c.mult with
      | 1., 0. -> None
      | const, mult -> Some (Fmt.str "%.2G * %2G" const mult)
    in
    let const = if c.add_const = 0.  then None
      else Some (Fmt.str "%.2G" c.add_const) in
    let per_tick = "/ ⏲️" in
    match exp, mult, const with
    | None, None, None -> ""
    | None, None, Some m | None, Some m, None | Some m, None, None ->
      Fmt.str "%s %s" m per_tick
    | Some a, Some b, None | None, Some a, Some b | Some a, None, Some b -> Fmt.str "(%s + %s) %s" a b per_tick
    | Some a, Some b, Some c -> Fmt.str "(%s + %s + %s) %s" a b c per_tick

let plus_lambda =
  let increase = 1. in
  {
    emoji = "λ";
    name = "write code";
    is_visible = always;
    is_usable = always;
    message = (fun _ -> Fmt.str "produce %G code" increase);
    action = (fun s ->
      {s with code = {s.code with amount = s.code.amount +. increase}});
    explanation = ["Knock out some code."]; (* TODO: something more poetic here *)
}

let beta_reduce =
  let cost s =
    let base_cost = 10. in
    if s.code.amount <= 100. then base_cost
    else base_cost +. (s.code.amount *. 0.05)
  in
  let quality_gain = 1. in
  {
    emoji = "🛠️";
    name = "refactor code";
    is_usable = (fun s -> s.code.amount >= (cost s));
    is_visible = (fun s -> s.code.amount >= 10. || s.quality.amount > 0. || s.hype.amount > 0. || s.camels.amount > 1. || s.reviewers.amount > 0.);
    message =
      (fun s -> Fmt.str "remove %G code and gain %G quality" (cost s) quality_gain);
    action = (fun s ->
      {s with code = {s.code with amount = s.code.amount -. (cost s)};
              quality = {s.quality with amount = s.quality.amount +. quality_gain}; 
      });
    explanation = [
"Not all code is good code.";
"Remove the cruft and keep the good stuff,";
"but the more code there is,";
"the harder the good stuff is to find...";
]
  }

let release =
  let quality_threshhold s =
    let hype_surcharge s = if s.cd.active then 6. else 10.05 in
    15. +. s.hype.amount *. (hype_surcharge s)
  in
  let hype_gain = 1. in
  {
    emoji = "📦";
    name = "release package";
    is_usable = (fun s -> s.quality.amount >= quality_threshhold s);
    is_visible = (fun s -> s.quality.amount >= quality_threshhold s || s.hype.amount > 0. || s.camels.amount > 1. || s.reviewers.amount > 0.);
    message = (fun s -> Fmt.str "spend %G quality and gain %G hype"
              (quality_threshhold s) hype_gain);
    action = (fun s -> { s with hype =
                            {s.hype with amount = s.hype.amount +. hype_gain };
                          quality =
                            {s.quality with amount = s.quality.amount -. quality_threshhold s; }
                      });
      explanation = [
        "Cut a new release with quality stuff in it.";
        "Everyone will be so excited!  But you can only";
        "brag about new features once; next time,";
        "you'll need even newer features.";
      ];
  }

let boost_add_const amount = function
  | Some change -> Some { change with add_const = change.add_const +. amount }
  | None -> Some { no_change with add_const = amount }

let contributors =
  let hype_threshhold s =
    let floor = 5. in
    let camels_adjustment = (s.camels.amount -. 1.) *. 1.40 in
    let docs_adjustment = s.docs.amount *. -0.05 in
    let adjusted = floor +. camels_adjustment +. docs_adjustment in
    if adjusted < floor then floor else adjusted
  in
  let contributor_gain = 1. in
  {
    name = "welcome contributors";
    emoji = "🐫";
    is_usable = (fun s -> s.hype.amount >= hype_threshhold s);
    is_visible = (fun s -> s.hype.amount > 0. || s.camels.amount > 1.);
    message = (fun s -> Fmt.str "spend %G hype and gain %G camel" (hype_threshhold s) contributor_gain);
    action = (fun s ->
        {s with code = {s.code with change = boost_add_const 0.1 s.code.change};
                camels = {s.camels with amount = s.camels.amount +. contributor_gain;
                                        visible = true;};
                hype = {s.hype with amount = s.hype.amount -. (hype_threshhold s)};
        }
      );
    explanation = [
    "People want to help out!";
    "Accept some contributions from your fellow camels.";
    "Many humps make light work!";
    "You'll be rolling in code before you know it.";
];
  }

let reviewers =
  (* it seems natural they should cost a lot of hype? *)
  let next_reviewer_cost s = 100. +. s.reviewers.amount *. 2. in
  let camel_cost = 1. in
  let reviewer_gain = 1. in
  {
    name = "bless reviewers";
    emoji = "👀";
    is_usable = (fun s -> s.camels.amount >= (camel_cost +. 1.)
                          && s.hype.amount >= next_reviewer_cost s);
    is_visible = (fun s -> s.camels.amount >= 2. || s.reviewers.amount > 0.);
    message = (fun s -> Fmt.str "spend %G hype and %G camel, gain %G reviewer" (next_reviewer_cost s) camel_cost reviewer_gain);
    action = (fun s ->
      { s with
        code = {s.code with change = boost_add_const ~-.1. s.code.change};
        reviewers = { s.reviewers with amount = s.reviewers.amount +. reviewer_gain;
                                       visible = true;
                    };
        hype = {s.hype with amount = s.hype.amount -. next_reviewer_cost s};
        camels = {s.camels with amount = s.camels.amount -. camel_cost};
        quality = {s.quality with change = boost_add_const 0.1 s.quality.change};
      });
    explanation = [
    "Refactoring all that code on your own is hard work!";
    "Find your most trusted fellow camel";
    "and ask them to help out with reviews and improvements.";
];
  }

let docs =
  (* the amount of docs you get decreases as there's more outstanding code. *)
  let increase s =
    let ceiling = 1. in
    let code_adjustment = (s.code.amount -. 100.) *. 0.01 in
    let adjusted = ceiling -. code_adjustment in
    if adjusted > ceiling then ceiling else if adjusted > 0. then adjusted else 0.
  in
  let active s = s.camels.amount > 1. || s.docs.amount > 0. in
  {
    name = "write docs";
    emoji = "🕮 ";
    is_visible = active;
    is_usable = active;
    message = (fun s -> Fmt.str "produce %G documentation" (increase s));
    action = (fun s -> {s with docs = {s.docs with amount = s.docs.amount +. increase s;
                                                   visible = true;}
                       }
             );
    explanation = [
    "Documentation makes the world go 'round.";
    "The more docs you have, the easier it is";
    "to attract new contributors to the project.";
    "Unfortunately, docs also become harder to write";
    "the more code there is."];
  }

let sparkle = {
  name = "ask sloppy";
  emoji = "✨";
  is_visible = (fun _ -> true);
  is_usable = (fun _ -> true);
  message = (fun _ -> "generate an unknown amount of code, of unknown quality");
  action = (fun s ->
      let roll = Random.int 10_000 in
      if roll <= 1000 then
        {s with code = {s.code with amount = s.code.amount +. (float_of_int roll /. 10.)};
                quality = {s.quality with amount = s.quality.amount -. (float_of_int roll /. 50.)}}
      else
        s
    );
  explanation = [
    "Why write code yourself? Try your luck";
    "with the latest and greatest in code";
    "generation tools! Now with extra";
    "sparkles!"
  ]
}

let all = [
  plus_lambda;
  sparkle;
  beta_reduce;
  release;
  contributors;
  reviewers;
  docs;
]

let max_explanation_width =
  let explanations = List.map (fun a -> a.explanation) all in
  let longest_line l = List.fold_left (fun most s -> max most (String.length s)) 0 l in
  List.fold_left (fun longest e -> max longest (longest_line e)) 0 explanations

(* later game content -- interactions with upstream? distribution? *)
  (* No. Later game content is clearly :sparkles: AI *)
