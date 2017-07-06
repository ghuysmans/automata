type ('sym, 'a) t
(* ('sym, 'a) t represents an automaton with 'sym symbols and 'a actions.
 * It won't block but just return to its initial state (implicitly 0).
 * There are no accepting states (since I won't use them). *)

val create: unit -> ('sym, 'a) t

val add: ('sym, 'a) t -> (int * 'sym * int * 'a list) -> unit
(* add x s t s' a adds a t/a transition from s to s' to x. *)

val step: ('sym, 'a) t -> 'sym -> 'a list
(* step x t processes an input t and returns the corresponding actions *)

val check: ('sym, 'a) t -> bool
(* tests whether the FSA is valid (no reference to any non-existing state *)

(* TODO repr with a tuple list, the first must contain the highest state id *)
