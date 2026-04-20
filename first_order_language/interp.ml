open Syntax

module Individual = struct
  type t = Lisa | Bart | Maggie

  let compare = compare

  module Set = struct
    include Set.Make (struct
      type nonrec t = t

      let compare = compare
    end)
  end
end

module StringMap = Map.Make (String)

type assignment = Individual.t StringMap.t

let domain = Individual.Set.of_list [ Lisa; Bart; Maggie ]
let empty : assignment = StringMap.empty

let eval_term (g : assignment) (term : term) : Individual.t =
  match term with
  | Const name -> (
      match name with
      | "Lisa" -> Lisa
      | "Bart" -> Bart
      | "Maggie" -> Maggie
      | _ -> failwith "Unknown individual constant")
  | Var x -> StringMap.find x g

let eval_unary_predicate (pred : string) : Individual.t -> bool =
 fun x ->
  match pred with
  | "female" -> ( match x with Lisa | Maggie -> true | _ -> false)
  | "exercises" -> ( match x with Lisa | Bart -> true | _ -> false)
  | "male" | "skateboards" -> ( match x with Bart -> true | _ -> false)
  | _ -> failwith "Unknown unary predicate"

let eval_binary_predicate (pred : string) : Individual.t * Individual.t -> bool
    =
  match pred with
  | "loves" -> (
      fun x -> match x with Lisa, Bart | Maggie, Lisa -> true | _ -> false)
  | _ -> failwith "Unknown binary predicate"

let rec eval (g : assignment) (formula : formula) : bool =
  match formula with
  | Neg p -> not (eval g p)
  | Conj (p, q) -> eval g p && eval g q
  | Dis (p, q) -> eval g p || eval g q
  | Cond (p, q) -> (not (eval g p)) || eval g q
  | Bicond (p, q) -> eval g p = eval g q
  | Eq (a, b) -> eval_term g a = eval_term g b
  | U_pred (p, a) -> (eval_unary_predicate p) (eval_term g a)
  | B_pred (p, a, b) -> (eval_binary_predicate p) (eval_term g a, eval_term g b)
  | Forall (x, p) ->
      Individual.Set.fold
        (fun k acc -> acc && eval (StringMap.add x k g) p)
        domain true
  | Exists (x, p) ->
      Individual.Set.fold
        (fun k acc -> acc || eval (StringMap.add x k g) p)
        domain false
