(* Head *)
type noun = N_proper of string | N_common of string
type verb = V of string

(* Phrase *)
type noun_phrase = NP of noun
type verb_phrase = VP_0 of verb | VP_1 of verb * noun_phrase

(* Sentence *)
type sentence = S of noun_phrase * verb_phrase
