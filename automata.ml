let string_of_state = function
  | Grammar.StateName s -> s
  | Grammar.StateIndex i -> string_of_int i

let () =
  let a = Fsa.create () in
  let lexbuf = Lexing.from_channel stdin in
  let rec f () =
    match Parser.top Lexer.top lexbuf with
    | Grammar.SymbolDefinition (n, s) ->
      print_endline n;
      f ()
    | Grammar.ActionDefinition (n, a) ->
      print_endline n;
      f ()
    | Grammar.StateDefinition (s, t) ->
      (*
      Fsa.add a (0, 'a', 0, [Print "hello"]);
      *)
      print_endline (string_of_state s);
      f () in
  if Array.length Sys.argv <> 2 || Sys.argv.(1) = "--help" then (
    Printf.fprintf stderr "usage: %s input_string\n" Sys.argv.(0);
    exit 1
  );
  try
    f ()
  with End_of_file ->
    let steps =
      let s = Sys.argv.(1) in
      Array.init (String.length s) (fun i -> s.[i]) |>
      Array.to_list in
    List.fold_left Action.run_step a steps |> ignore
