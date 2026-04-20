type term = Const of string | Var of string

type formula =
  | Eq of term * term
  | U_pred of string * term
  | B_pred of string * term * term
  | Neg of formula
  | Conj of formula * formula
  | Dis of formula * formula
  | Cond of formula * formula
  | Bicond of formula * formula
  | Forall of string * formula
  | Exists of string * formula

let string_of_term (term : term) =
  match term with Const name -> name | Var x -> x

let rec string_of_formula (formula : formula) =
  match formula with
  | Eq (a, b) -> string_of_term a ^ " = " ^ string_of_term b
  | U_pred (p, a) -> p ^ "(" ^ string_of_term a ^ ")"
  | B_pred (p, a, b) ->
      p ^ "(" ^ string_of_term a ^ ", " ^ string_of_term b ^ ")"
  | Neg p -> "¬" ^ string_of_formula p
  | Conj (p, q) -> string_of_formula p ^ " ∧ " ^ string_of_formula q
  | Dis (p, q) -> string_of_formula p ^ " ∨ " ^ string_of_formula q
  | Cond (p, q) -> string_of_formula p ^ " → " ^ string_of_formula q
  | Bicond (p, q) -> string_of_formula p ^ " ↔︎ " ^ string_of_formula q
  | Forall (u, p) -> "∀" ^ u ^ "[" ^ string_of_formula p ^ "]"
  | Exists (u, p) -> "∃" ^ u ^ "[" ^ string_of_formula p ^ "]"
