open Formal_english

let example1 : Syntax.sentence =
  S (NP (N' (N_proper "Bart")), VP_i (V "exercises"))

let example2 : Syntax.sentence =
  S (NP (N' (N_proper "Lisa")), VP_i (V "skateboards"))

let example3 : Syntax.sentence =
  S (NP (N' (N_proper "Homer")), VP_t (V "shot", NP (N' (N_proper "Burns"))))

let print_result (sentence : Syntax.sentence) : unit =
  sentence |> Interp.eval |> string_of_bool |> print_endline

let () =
  print_result example1;
  print_result example2;
  print_result example3
