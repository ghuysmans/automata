let compile s =
  let compile_automaton fn =
    let ch = open_in fn in
    let a, assignments = Lexing.from_channel ch |> Compiler.compile in
    close_in ch;
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
