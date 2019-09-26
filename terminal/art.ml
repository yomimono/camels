let image_of_ascii attr s =
 (* line-by-line compose each of these into a string image *)
  String.split_on_char '\n' s |> List.map (Notty.I.string attr) |> Notty.I.vcat

let splash = {|
        ----        ----
       /    \      /    \
      /      \    /      \        /-\
     /        \  /        \      /   ------
    /          --          \     |         \
 ---                        \    |      -   |
/                            -___/     / \__|
                                      |
 ^                                    /
/ \                                  /
   -_______________  ___  __________/
                  / /   \ \
                 / /     \ \
                / /       \ \
               / /         \ \
              / /           \ \
             / /             \ \
|} |> image_of_ascii Notty.A.empty
