type state = int
type ('sym, 'a) t
(* ('sym, 'a) t represents an automaton with 'sym symbols and 'a actions.
 * It won't block but just return to its initial state (implicitly 0).
 * There are no accepting states (since I won't use them). *)

val create: unit -> ('sym, 'a) t

val add: ('sym, 'a) t -> (int * 'sym * int * 'a list) -> unit
(* add x s t s' a adds a t/a transition from s to s' to x. *)

val step: ('sym -> 'syms -> bool) -> ('syms, 'a) t -> 'sym -> 'a list
(* step test x t processes an input t and returns the corresponding actions *)

val closure: _ t -> state -> [> `Invalid of state | `Visited of state list ]
(* closure x i computes the transitive closure of x starting from state i. *)
