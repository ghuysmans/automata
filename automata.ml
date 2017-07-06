type action =
  | Print of string

let run = function
  | Print s -> print_endline s

let run_step x sym =
  Fsa.step x sym |>
  List.iter run;
  x

let () =
  let a = Fsa.create () in
  (* TODO parser *)
  Fsa.add a (0, 'a', 0, [Print "hello"]);
  let steps = ['a'; 'a'; 'a'] in
  List.fold_left run_step a steps |> ignore
