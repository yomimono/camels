(* change per tick for a value:
 * exp_const ^^ exp + mult_const * mult + add_const *)
(* TODO: float is almost certainly wrong for this;
 * we need bignum support to be a proper incremental *)
type tick_change = {
  exp_const : float;
  exp : float;
  mult_const : float;
  mult : float;
  add_const : float;
} [@@deriving yojson]

type item = {
  amount : float;
  change : tick_change option;
  visible : bool;
  last_amounts : float list;
} [@@deriving yojson]

type process = {
  active : bool;
  visible : bool;
} [@@deriving yojson]

type state = {
  lets_bail : bool;
  graph_max : int;
  graph_interval : int;
  last_tick : int64;
  code : item;
  quality : item;
  hype : item;
  camels : item;
  docs : item;
  reviewers : item;
  downstream : item;
  ci : process;
  cd : process;
  ticks : int;
} [@@deriving yojson]

type control = {
  action : state -> state;
  is_visible : state -> bool;
  is_usable : state -> bool;
  message : state -> string; (* what immediate effect does doing this have? *)
  name : string;
  emoji : string;
  explanation : string list; (* some flavor text about the action and its strategy *)
}

