type 'a t =
  | Simple of 'a
  | Sequence of 'a t list (* yields the 1st action list if it's not [] *)
  | Union of 'a t list (* keep both action lists (left then right) *)

val dump: string t -> string t
(* dump t dumps the input and returns it unchanged *)

val step: ('sym -> 'syms -> bool) -> ('syms, 'a) Fsa.t t -> 'sym -> 'a list
(* step test x t processes an input t and returns the corresponding actions *)
