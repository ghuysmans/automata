type ('sym, 'a) t =
  | Simple of ('sym, 'a) Fsa.t
  | First of ('sym, 'a) t list (* yields the 1st action list if it's not [] *)
  | Both of ('sym, 'a) t list (* keep both action lists (left then right) *)

val step: ('sym, 'a) t -> 'sym -> 'a list
(* step x t processes an input t and returns the corresponding actions *)
