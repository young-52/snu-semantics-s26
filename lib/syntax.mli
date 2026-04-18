(* Head *)
type noun = N_proper of string | N_common of string
type verb = V of string
type adjective = Adj_inter of string | Adj_non of string
type determiner = Det of string

(* Intermediate projection *)
type noun' = N' of noun | N'_mod of adjective * noun'

(* Phrase *)
type noun_phrase = NP of noun' | NP_d of determiner * noun'
type verb_phrase = VP_i of verb | VP_t of verb * noun_phrase

(* Sentence *)
type sentence = S of noun_phrase * verb_phrase

(* Stringify *)
val string_of_sentence : sentence -> string
