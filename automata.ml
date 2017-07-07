let () =
  let a = Fsa.create () in
  (* TODO parser *)
  Fsa.add a (0, 'a', 0, [Print "hello"]);
  let steps = ['a'; 'a'; 'b'; 'a'] in
  List.fold_left Action.run_step a steps |> ignore
