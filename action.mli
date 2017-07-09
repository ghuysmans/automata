type t =
  | Print of string
  | Broadcast of int * string

val run: t -> unit
