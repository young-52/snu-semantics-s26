open Formal_english

let examples : Syntax.sentence list =
  [
    S (NP (N' (N_proper "Bart")), VP_i (V "exercises"));
    S (NP (N' (N_proper "Lisa")), VP_i (V "skateboards"));
    S (NP (N' (N_proper "Homer")), VP_t (V "shot", NP (N' (N_proper "Burns"))));
    S (NP_d (Det "The", N' (N_common "boss")), VP_i (V "skateboards"));
    S (NP_d (Det "The", N' (N_common "parent")), VP_i (V "exercises"));
    S
      ( NP_d (Det "The", N'_mod (Adj_inter "female", N' (N_common "parent"))),
        VP_i (V "exercises") );
  ]

let print_result (sentence : Syntax.sentence) : unit =
  let syntax = Syntax.string_of_sentence sentence in
  let int_of_bool = fun x -> match x with true -> 1 | false -> 0 in
  let extension =
    match Interp.eval sentence with
    | x -> x |> int_of_bool |> string_of_int
    | exception Interp.Undefined -> "undefined"
  in
  "⟦" ^ syntax ^ ".⟧ = " ^ extension |> print_endline

let () = List.iter print_result examples
