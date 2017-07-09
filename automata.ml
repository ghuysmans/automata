let run_step x sym =
  Fsa.step Symbol.test x sym |>
  List.iter Action.run;
  x

let () =
  if Array.length Sys.argv <> 2 || Sys.argv.(1) = "--help" then (
    Printf.fprintf stderr "usage: %s input_string\n" Sys.argv.(0);
    exit 1
  );
  let lexbuf = Lexing.from_channel stdin in
  let a, assignments = Compiler.compile lexbuf in
  List.iter (fun (n, i) -> Printf.printf "%s -> %d\n" n i) assignments;
  let steps =
    let s = Sys.argv.(1) in
    Array.init (String.length s) (fun i -> s.[i]) in
  Array.fold_left run_step a steps |> ignore
