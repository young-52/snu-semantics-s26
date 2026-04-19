type term = Const of string

type formula =
  | Eq of term * term
  | U_pred of string * term
  | B_pred of string * term * term
  | Neg of formula
  | Conj of formula * formula
  | Dis of formula * formula
  | Cond of formula * formula
  | Bicond of formula * formula

val string_of_formula : formula -> string
