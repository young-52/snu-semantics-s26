open Syntax

exception Undefined

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

let eval_intersective_adjective (adj : string) : Entity.Set.t =
  Entity.Set.of_list
    (match adj with
    | "streetsmart" -> [ Maggie; Bart ]
    | "vegetarian" -> [ Lisa ]
    | "American" -> [ Maggie; Lisa; Bart; Marge; Homer; Burns ]
    | "female" -> [ Maggie; Lisa; Marge ]
    | _ -> failwith "Not intersective adjective in this world")

let eval_nonintersective_adjective (adj : string) : Entity.Set.t -> Entity.Set.t
    =
  match adj with
  | "skillful" -> (
      fun x ->
        match Entity.Set.to_list x with
        | [ Marge; Homer ] -> Entity.Set.singleton Marge
        | [ Burns ] -> Entity.Set.singleton Burns
        | [ Lisa; Homer ] -> Entity.Set.singleton Lisa
        | [ Lisa; Homer; Bart ] -> Entity.Set.of_list [ Lisa; Bart ]
        | _ -> failwith "Impossible in this world")
  | _ -> failwith "Not non-intersective adjective in this world"

let rec eval_modified_n' (n' : noun') : Entity.Set.t =
  match n' with
  | N' (N_common noun) -> eval_common_noun noun
  | N'_mod (Adj_inter adj, n') ->
      Entity.Set.inter (eval_intersective_adjective adj) (eval_modified_n' n')
  | N'_mod (Adj_non adj, n') ->
      (eval_nonintersective_adjective adj) (eval_modified_n' n')
  | N' (N_proper _) -> failwith "No proper noun here"

let eval_the_n' (n' : noun') : Entity.t option =
  match n' with
  | N' (N_common noun) -> (
      match Entity.Set.to_list (eval_common_noun noun) with
      | [ x ] -> Some x
      | _ -> None)
  | N'_mod _ -> (
      match Entity.Set.to_list (eval_modified_n' n') with
      | [ x ] -> Some x
      | _ -> None)
  | N' (N_proper _) -> None

let eval_np_d (np : noun_phrase) : Entity.t option =
  match np with
  | NP_d (det, n') -> (
      match det with
      | Det ("The" | "the") -> eval_the_n' n'
      | _ -> failwith "Determiner not exists in this world")
  | _ -> failwith "Determiner not exists"

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
  | VP_i (V verb) -> eval_intrnasitive_verb verb
  | VP_t (V verb, np) -> (
      let sem_of_verb = eval_transitive_verb verb in
      match np with
      | NP (N' (N_proper name)) ->
          let sem_of_obj = eval_proper_name name in
          sem_of_verb
          |> Entity_pair.Set.filter (fun (_, obj) -> obj = sem_of_obj)
          |> fun acc ->
          Entity_pair.Set.fold
            (fun (subj, _) acc -> Entity.Set.add subj acc)
            acc Entity.Set.empty
      | NP_d _ -> (
          let sem_of_obj = eval_np_d np in
          match sem_of_obj with
          | Some x ->
              sem_of_verb |> Entity_pair.Set.filter (fun (_, obj) -> obj = x)
              |> fun acc ->
              Entity_pair.Set.fold
                (fun (subj, _) acc -> Entity.Set.add subj acc)
                acc Entity.Set.empty
          | None -> raise Undefined)
      | _ -> failwith "Unimplemented")

let eval (sentence : sentence) : bool =
  let (S (np, vp)) = sentence in
  match np with
  | NP (N' (N_proper name)) ->
      Entity.Set.mem (eval_proper_name name) (eval_vp vp)
  | NP (N' (N_common noun)) ->
      Entity.Set.subset (eval_common_noun noun) (eval_vp vp)
  | NP_d _ -> (
      let entity = eval_np_d np in
      match entity with
      | Some x -> Entity.Set.mem x (eval_vp vp)
      | None -> raise Undefined)
  | NP (N'_mod _) -> failwith "NP modification unimplemented"
