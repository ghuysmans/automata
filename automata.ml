let compile s =
  let compile_automaton fn =
    let ch = open_in fn in
    let a, assignments = Lexing.from_channel ch |> Compiler.compile in
    close_in ch;
    Printf.printf "compiled %s:\n" fn;
    List.iter (fun (n, i) -> Printf.printf "%s -> %d\n" n i) assignments;
    a in
  let rec convert = function
    | Combinators.Simple fn -> Combinators.Simple (compile_automaton fn)
    | Combinators.Sequence l -> Combinators.Sequence (List.map convert l)
    | Combinators.Union l -> Combinators.Union (List.map convert l) in
  Lexing.from_string s |>
  Combinator_parser.top Combinator_lexer.top |>
  convert

let run_step t sym =
  Combinators.step Symbol.test t sym |>
  List.iter Action.run;
  t

let () =
  if Array.length Sys.argv <> 3 then (
    Printf.fprintf stderr "usage: %s automata input\n" Sys.argv.(0);
    exit 1
  );
  let t = compile Sys.argv.(1) in
  let steps =
    let s = Sys.argv.(2) in
    Array.init (String.length s) (fun i -> s.[i]) in
  Array.fold_left run_step t steps |> ignore
