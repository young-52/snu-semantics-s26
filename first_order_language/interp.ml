open Syntax

type entity = Lisa | Bart | Maggie

let eval_term (term : term) : entity =
  match term with
  | Const "Lisa" -> Lisa
  | Const "Bart" -> Bart
  | Const "Maggie" -> Maggie
  | _ -> failwith "Unknown term"

let eval_unary_predicate (pred : string) : entity -> bool =
 fun x ->
  match pred with
  | "exercises" -> ( match x with Lisa | Bart -> true | _ -> false)
  | "skateboards" -> ( match x with Bart -> true | _ -> false)
  | _ -> failwith "Unknown unary predicate"

let eval_binary_predicate (pred : string) : entity * entity -> bool =
  match pred with
  | "loves" -> (
      fun x -> match x with Lisa, Bart | Maggie, Lisa -> true | _ -> false)
  | _ -> failwith "Unknown binary predicate"

let rec eval (formula : formula) : bool =
  match formula with
  | Neg p -> not (eval p)
  | Conj (p, q) -> eval p && eval q
  | Dis (p, q) -> eval p || eval q
  | Cond (p, q) -> (not (eval p)) || eval q
  | Bicond (p, q) -> eval p = eval q
  | Eq (a, b) -> eval_term a = eval_term b
  | U_pred (p, a) -> (eval_unary_predicate p) (eval_term a)
  | B_pred (p, a, b) -> (eval_binary_predicate p) (eval_term a, eval_term b)
