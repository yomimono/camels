open Notty
open Notty.Infix

let fill_char = Uchar.of_int 0x2588
and off_charts_up = Uchar.of_int 0x2191
and off_charts_down = Uchar.of_int 0x2193
and empty_char = ' '

let one attr uchar = I.uchar attr uchar 1 1

module Int = struct
(** [vertical_bar_chart ?attr ~height ~scale ~y_min values]
 * produces a bar chart representing [values], with each
 * line equal to [scale] from [y_min] to [height].
 * Negative values are depicted in a surprising and broken manner;
 * rather than descending from the line at y=0,
 * they ascend from below the graph to meet the point on the y axis
 * matching their value.
 *)
  let y_max ~height ~scale ~y_min = (height - 1) * scale + y_min
  
  let graph ~attr ~y_min ~scale ~height amount =
    let partial_fill y_min scale amount =
      let n = (amount - y_min) mod scale in
      if n = 0 then I.char attr empty_char 1 1
      else if n * 8 < scale then I.char attr ' ' 1 1 (* under an eighth, no display *)
      else if n * 4 < scale then I.uchar attr (Uchar.of_int 0x2581) 1 1
      else if (n * 8) / 3 < scale then I.uchar attr (Uchar.of_int 0x2582) 1 1
      else if n * 2 < scale then I.uchar attr (Uchar.of_int 0x2583) 1 1
      else if (n * 8) / 5 < scale then I.uchar attr (Uchar.of_int 0x2584) 1 1
      else if (n * 4) / 3 < scale then I.uchar attr (Uchar.of_int 0x2585) 1 1
      else if (n * 8) / 7 < scale then I.uchar attr (Uchar.of_int 0x2586) 1 1
      else I.uchar attr (Uchar.of_int 0x2587) 1 1
    in
    let y_max = y_max ~height ~scale ~y_min in
    let bounds line_number =
      let max = y_max - (scale * line_number) in
      let min = max - scale in
      (min, max)
    in
    let rec line_fill amount = function
      | 0 when amount > y_max ->
        (one attr off_charts_up) :: line_fill amount 1
      | line_number when line_number > (height - 1) -> []
      | line_number when amount >= y_max - (line_number * scale) ->
        (one attr fill_char) :: line_fill amount (line_number + 1)
      | line_number when line_number = height - 1 && amount < y_min ->
        (one attr off_charts_down) ::  []
      | line_number when snd @@ bounds line_number < amount -> (one attr fill_char) :: line_fill amount (line_number + 1)
      | line_number when fst @@ bounds line_number > amount ->
        (I.char attr ' ' 1 1) :: line_fill amount (line_number + 1)
      | line_number -> partial_fill y_min scale amount :: line_fill amount (line_number + 1)
    in
    I.vcat @@ line_fill amount 0

  let y_axis ~height ~scale ~y_min attr =
    let border = I.uchar attr (Uchar.of_int 0x250b) 1 height in
    let corner = I.uchar attr (Uchar.of_int 0x2517) 1 1 in
    let y_max = y_max ~height ~scale ~y_min in
    let rec y_axis_labels n =
      if n <= y_min then y_min :: []
      else n :: y_axis_labels (n-scale)
    in
    let labels = List.fold_left (fun labels row ->
        labels <-> Notty.I.string attr @@ string_of_int row) I.empty (y_axis_labels y_max)
    in
    labels  <|> (border <-> corner)

  let vertical ?(attr = A.empty) ~height ~scale ~y_min amounts =
    let make_graph = graph ~attr ~y_min ~scale ~height in
    let graph = List.fold_left (fun charted amount -> charted <|> make_graph amount) I.empty amounts in
    let x_axis = I.uchar attr (Uchar.of_int 0x2501) (List.length amounts) 1 in
    let y_axis = y_axis ~height ~scale ~y_min attr in
    y_axis <|> (graph <-> x_axis)

  (* in this case, attr governs only the axes/labels *)
  let vertical_attr ?(attr = A.empty) ~height ~scale ~y_min amounts =
    let make_graph = (fun (amount, attr) -> graph ~attr ~y_min ~scale ~height amount) in
    let graph = List.fold_left (fun charted a -> charted <|> make_graph a) I.empty amounts in
    let x_axis = I.uchar attr (Uchar.of_int 0x2501) (List.length amounts) 1 in
    let y_axis = y_axis ~height ~scale ~y_min attr in
    y_axis <|> (graph <-> x_axis)
end
