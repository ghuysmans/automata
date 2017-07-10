let () =
  if Array.length Sys.argv <> 2 then (
    Printf.fprintf stderr "usage: %s fsa\n" Sys.argv.(0);
    exit 1
  );
  let ch =
    let fn = Sys.argv.(1) in
    if fn = "-" then
      stdin
    else
      open_in fn in
  let a, assignments = Compiler.compile (Lexing.from_channel ch) in
  Fsa.dot Action.to_string Symbol.to_string stdout assignments a;
  close_in ch
