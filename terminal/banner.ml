open Idle.S
module State = Idle.State

let header =
  let open Notty.Infix in
  let name = Notty.(I.string A.(st underline) "activity") in
  let color n =
    let r, b = n, n in
    Notty.(I.uchar 
    A.(fg @@ rgb ~r ~g:5 ~b) (Uchar.of_int 0x2588) 1 1)
  in
  let lightest = Notty.(I.string A.empty "least: " <|> color 5)
  and darkest = Notty.(I.string A.empty "most: " <|> color 0)
  and sep = Notty.(I.string A.empty "; ") in
  Notty.I.hcat [ name; Notty.I.void 5 0; lightest; sep; darkest ]

let banner s =
  let height = 7 in
  (* we want to give something like GitHub's commit graph,
   * where actions we take give heat maps over ticks. *)
  (* for now, we can just graph abs (this_code - last_code) + abs(this_quality - last_quality) + abs(this_dos - last_docs), I suppose? *)
  let find_change = function
    | [] -> []
    | hd::[] -> hd :: []
    | l ->
    (* prevs is l with the first item removed;
     * the nth item in prevs corresponds to the (n - 1)th item in i *)
    let prev = List.tl l in
    (* and then we need to not attempt to find the corresponding item at the end of l *)
    let l = List.(rev @@ tl @@ rev l) in
    List.map2 (fun c p -> Float.(abs @@ sub c p)) l prev
  in
  let code_changes = find_change s.code.last_amounts
  and quality_changes = find_change s.quality.last_amounts
  and docs_changes = find_change s.docs.last_amounts
  in
  let changes = List.map2 (+.) code_changes quality_changes in
  let changes = List.map2 (+.) changes docs_changes in
  let min, max = State.get_min_max changes in
  (* "all three channels must be in the range 0-5", so we have 6 possible gradations *)
  let scale = (max -. min) /. 6. in
  let scale = if scale <= 0. then min else scale in

  let g = 5 in
  let heat_map ~min ~scale i' =
    (* if no commits happened, the block should be empty in the commit map *)
    if i' <= 0. then Notty.I.void 1 1
    else begin
    let other_colors =
      let i = i' -. min in
      if scale *. 1. >= i then 5
      else if scale *. 2. >= i then 4
      else if scale *. 3. >= i then 3
      else if scale *. 4. >= i then 2
      else if scale *. 5. >= i then 1
      else 0
    in
    let r, b = other_colors, other_colors in
    Notty.(I.uchar A.(fg @@ rgb ~r ~g ~b) (Uchar.of_int 0x2588) 1 1)
  end
  in
  (* we want sets of 7 columns each; for now, we'll make one *)
  (* this would be much easier to do using the tabulate interface to make
   * an image; we should rewrite it that way *)
  let width = s.graph_max / height in
  let show_heatmap x y =
    let x = (width - 1) - x in
    match List.nth_opt changes (x*height + y) with
    | None -> Notty.I.void 1 1
    | Some i -> heat_map ~min ~scale i
  in
  Notty.(I.vcat [
      header;
      I.tabulate width height show_heatmap])
