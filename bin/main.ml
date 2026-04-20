module FE = Formal_english.Syntax
module FOL = First_order_language.Syntax

let fe_examples : FE.sentence list =
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

let fol_examples : FOL.formula list =
  [
    Conj
      ( U_pred ("exercises", Const "Bart"),
        Neg (U_pred ("exercises", Const "Lisa")) );
    Cond
      ( U_pred ("skateboards", Const "Bart"),
        B_pred ("loves", Const "Bart", Const "Lisa") );
    Forall ("x", Cond (U_pred ("male", Var "x"), U_pred ("exercises", Var "x")));
    Exists
      ("x", Conj (U_pred ("female", Var "x"), U_pred ("exercises", Var "x")));
  ]

let int_of_bool : bool -> int = fun x -> match x with true -> 1 | false -> 0

let print_fe_result (sentence : FE.sentence) : unit =
  let open Formal_english.Interp in
  let syntax = FE.string_of_sentence sentence in
  let extension =
    match eval sentence with
    | x -> x |> int_of_bool |> string_of_int
    | exception Undefined -> "undefined"
  in
  "⟦" ^ syntax ^ ".⟧ = " ^ extension |> print_endline

let print_fol_result (formula : FOL.formula) : unit =
  let open First_order_language.Interp in
  let syntax = FOL.string_of_formula formula in
  let extension = formula |> eval empty |> int_of_bool |> string_of_int in
  "⟦" ^ syntax ^ "⟧ = " ^ extension |> print_endline

let () =
  print_endline "============== Formal English ==============";
  print_newline ();
  List.iter print_fe_result fe_examples;
  print_newline ();
  print_endline "=========== First Order Language ===========";
  print_newline ();
  List.iter print_fol_result fol_examples
