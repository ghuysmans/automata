let () =
  if Array.length Sys.argv <> 3 then (
    Printf.fprintf stderr "usage: %s automata input\n" Sys.argv.(0);
    exit 1
  );
  let t = Automata.compile Sys.argv.(1) in
  let steps =
    let s = Sys.argv.(2) in
    Array.init (String.length s) (fun i -> s.[i]) in
  Array.fold_left Automata.run_step t steps |> ignore
