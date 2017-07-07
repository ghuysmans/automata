type t =
  | Print of string

let run = function
  | Print s -> print_endline s

let run_step x sym =
  Fsa.step x sym |>
  List.iter run;
  x
