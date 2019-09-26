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
(* TODO: this is wrong when CI and CD are activated *)
let pp_change = function
  | None -> Fmt.strf ""
  | Some c ->
    let exp = match c.exp_const, c.exp with
      | 0., 1. -> None 
      | const, exp -> Some (Fmt.strf "%.2G ^ %.2G" const exp)
    in
    let mult = match c.mult_const, c.mult with
      | 1., 0. -> None
      | const, mult -> Some (Fmt.strf "%.2G * %2G" const mult)
    in
    let const = if c.add_const = 0.  then None
      else Some (Fmt.strf "%.2G" c.add_const) in
    let per_tick = "/ ⏲️" in
    match exp, mult, const with
    | None, None, None -> ""
    | None, None, Some m | None, Some m, None | Some m, None, None ->
      Fmt.strf "%s %s" m per_tick
    | Some a, Some b, None | None, Some a, Some b | Some a, None, Some b -> Fmt.strf "(%s + %s) %s" a b per_tick
    | Some a, Some b, Some c -> Fmt.strf "(%s + %s + %s) %s" a b c per_tick

let plus_lambda =
  let increase = 1. in
  {
    emoji = "λ";
    name = "write code";
    is_visible = always;
    is_usable = always;
    message = (fun _ -> Fmt.strf "produce %G code" increase);
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
      (fun s -> Fmt.strf "remove %G code and gain %G quality" (cost s) quality_gain);
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
    message = (fun s -> Fmt.strf "spend %G quality and gain %G hype"
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
    message = (fun s -> Fmt.strf "spend %G hype and gain %G contributor" (hype_threshhold s) contributor_gain);
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
    message = (fun s -> Fmt.strf "spend %G hype and %G camel, gain %G reviewer" (next_reviewer_cost s) camel_cost reviewer_gain);
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
    message = (fun s -> Fmt.strf "produce %G documentation" (increase s));
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

let ci =
(* CI makes each of your camels contribute a small amount of quality. *)
  {
    emoji = "🤖";
    name = "set up ci";
    is_visible = (fun s -> (s.camels.amount +. s.reviewers.amount) > 100. || s.ci.active);
    is_usable = (fun s -> (s.camels.amount +. s.reviewers.amount) > 100. && not s.ci.active);
    message = (fun _ -> "each camel will contribute some quality");
    action = (fun s -> {s with ci = {s.ci with active = true;};});
    explanation = [
      "We can't make sure that";
      "each line of code is a good one,";
      "but we can at least make sure";
      "they don't break the build.  ...right?"];
  }

(* TODO: CD makes quality directly convert to hype when possible *)
(* for now, CD makes hype cheaper *)
let cd =
  {
    name = "set up cd";
    emoji = "🔁🤖";
    is_visible = (fun s -> (s.camels.amount +. s.reviewers.amount) > 200. || s.cd.active);
    is_usable = (fun s -> (s.camels.amount +. s.reviewers.amount) > 100. && not s.cd.active);
    message = (fun _ -> "releasing packages costs less quality");

    action = (fun s -> {s with cd = {s.cd with active = true;};});
    explanation = ["So many new features,";
                   "so many bugfixes...";
                   "releases are a lot of work.";
                   "Try to apply some automation."] (* TODO this really doesn't work thematically *)
  }

(* later game content -- interactions with upstream? distribution? *)
