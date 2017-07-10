let () =
  if Array.length Sys.argv <> 4 then (
    Printf.fprintf stderr "usage: %s automata listen shout\n" Sys.argv.(0);
    exit 1
  );
  let shout s =
    let port = int_of_string Sys.argv.(3) in
    Udp.send port s in
  let try_compile e =
    try
      let x = Automata.compile e in
      shout "loaded";
      Some x
    with _ ->
      shout "failed";
      None in
  let t = ref (try_compile Sys.argv.(1)) in
  Udp.server (int_of_string Sys.argv.(2)) 64 (fun s ->
    let s = String.trim s in
    if s = "" then
      ()
    else
      let r = String.sub s 1 (String.length s - 1) in
      if s.[0] = 'c' then (
        Unix.chdir r;
        shout "chdired"
      )
      else if s.[0] = 'l' then
        t := try_compile r
      else if s.[0] = 'k' then (
        shout "bye";
        exit 0
      )
      else if String.length s > 1 && s.[0] = 'r' then
        match !t with
        | Some t' -> t := Some (Automata.run_step t' s.[1])
        | None -> ()
      else
        shout "wut?"
  )
