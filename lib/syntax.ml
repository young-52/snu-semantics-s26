(* Head *)
type noun = N_proper of string | N_common of string
type verb = V of string
type adjective = Adj_inter of string | Adj_non of string
type determiner = Det of string | Det_q of string

(* Intermediate projection *)
type noun' = N' of noun | N'_mod of adjective * noun'

(* Phrase *)
type noun_phrase = NP of noun' | NP_d of determiner * noun'
type verb_phrase = VP_i of verb | VP_t of verb * noun_phrase

(* Sentence *)
type sentence = S of noun_phrase * verb_phrase

let string_of_n (n : noun) : string =
  match n with N_proper x | N_common x -> x

let string_of_v (v : verb) : string = match v with V x -> x

let string_of_adj (adj : adjective) : string =
  match adj with Adj_inter x | Adj_non x -> x

let string_of_det (det : determiner) : string =
  match det with Det x | Det_q x -> x

let rec string_of_n' (n' : noun') : string =
  match n' with
  | N' n -> string_of_n n
  | N'_mod (adj, n') -> string_of_adj adj ^ " " ^ string_of_n' n'

let string_of_np (np : noun_phrase) : string =
  match np with
  | NP n' -> string_of_n' n'
  | NP_d (det, n') -> string_of_det det ^ " " ^ string_of_n' n'

let string_of_vp (vp : verb_phrase) : string =
  match vp with
  | VP_i v -> string_of_v v
  | VP_t (v, np) -> string_of_v v ^ " " ^ string_of_np np

let string_of_sentence (sentence : sentence) : string =
  match sentence with S (np, vp) -> string_of_np np ^ " " ^ string_of_vp vp
