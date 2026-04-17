open Syntax

module Entity = struct
  type t = Maggie | Lisa | Bart | Marge | Homer | Burns

  let compare = compare

  module Set = struct
    include Set.Make (struct
      type nonrec t = t

      let compare = compare
    end)
  end
end

module Entity_pair = struct
  type t = Entity.t * Entity.t

  let compare = compare

  module Set = struct
    include Set.Make (struct
      type nonrec t = t

      let compare = compare
    end)
  end
end

let eval_proper_name (name : string) : Entity.t =
  match name with
  | "Maggie" -> Maggie
  | "Lisa" -> Lisa
  | "Bart" -> Bart
  | "Marge" -> Marge
  | "Homer" -> Homer
  | "Burns" -> Burns
  | _ -> failwith "Proper name not exists in this world"

let eval_common_noun (noun : string) : Entity.Set.t =
  Entity.Set.of_list
    (match noun with
    | "Simpson" -> [ Maggie; Lisa; Bart; Marge; Homer ]
    | "child" -> [ Maggie; Lisa; Bart ]
    | "boss" -> [ Burns ]
    | "parent" -> [ Marge; Homer ]
    | "female" -> [ Marge; Lisa; Maggie ]
    | "instrumentalist" -> [ Marge; Homer; Bart ]
    | "saxophonist" -> [ Lisa; Homer ]
    | _ -> failwith "Common noun not exists in this world")

let eval_intrnasitive_verb (verb : string) : Entity.Set.t =
  Entity.Set.of_list
    (match verb with
    | "exercises" -> [ Bart; Homer; Marge; Lisa ]
    | "skateboards" -> [ Bart; Homer ]
    | "shot Burns" -> [ Maggie ]
    | _ -> failwith "Intransitive verb not exists in this world")

let eval_transitive_verb (verb : string) : Entity_pair.Set.t =
  Entity_pair.Set.of_list
    (match verb with
    | "shot" -> [ (Maggie, Burns); (Bart, Homer) ]
    | _ -> failwith "Transitive verb not exists in this world")

let eval_vp (vp : verb_phrase) : Entity.Set.t =
  match vp with
  | VP_0 (V verb) -> eval_intrnasitive_verb verb
  | VP_1 (V verb, np) -> (
      let sem_of_verb = eval_transitive_verb verb in
      match np with
      | NP (N_proper name) ->
          let sem_of_obj = eval_proper_name name in
          sem_of_verb
          |> Entity_pair.Set.filter (fun (_, obj) -> obj = sem_of_obj)
          |> fun acc ->
          Entity_pair.Set.fold
            (fun (subj, _) acc -> Entity.Set.add subj acc)
            acc Entity.Set.empty
      | NP (N_common _) -> failwith "Unimplemented")

let eval (sentence : sentence) : bool =
  let (S (np, vp)) = sentence in
  match np with
  | NP (N_proper name) -> Entity.Set.mem (eval_proper_name name) (eval_vp vp)
  | NP (N_common noun) -> Entity.Set.subset (eval_common_noun noun) (eval_vp vp)
