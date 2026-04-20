open Syntax

module Individual : sig
  type t
end

module StringMap : sig
  type 'a t
end

type assignment = Individual.t StringMap.t

val empty : assignment
val eval : assignment -> formula -> bool
