let rec expand_symbol env s = match s with
  | Grammar.SymbolRange _ -> s
  | Grammar.Symbol _ -> s
  | Grammar.SymbolMacro m ->
    if List.mem_assoc m env then
      List.assoc m env
    else
      failwith @@ "undefined symbol macro " ^ m
  | Grammar.SymbolUnion l ->
    Grammar.SymbolUnion (List.map (expand_symbol env) l)

let rec expand_actions env = function
  | [] -> []
  | (Grammar.Action a) :: r -> a :: expand_actions env r
  | Grammar.ActionMacro m :: r ->
    if List.mem_assoc m env then
      List.assoc m env @ expand_actions env r
    else
      failwith @@ "undefined action macro " ^ m

let string_of_state = function
  | Grammar.StateName s -> s
  | Grammar.StateIndex i -> string_of_int i

let next_max_state m s =
  let f a (_, i, _) = match i with
    | Grammar.StateName _ -> a
    | Grammar.StateIndex i' -> max a i' in
  List.fold_left f m s

let assign_ids (states, max) =
  let max_state = ref max in
  let assignments = ref [] in
  let assign = function
    | Grammar.StateIndex i -> i
    | Grammar.StateName n ->
      if List.mem_assoc n !assignments then
        List.assoc n !assignments
      else (
        incr max_state;
        assignments := (n, !max_state) :: !assignments;
        !max_state
      ) in
  let f (i, t) =
    let assign_in_transition (s, i', a) = s, assign i', a in
    assign i, List.map assign_in_transition t in
  List.map f states, !assignments

let parse lexbuf =
  let max_state = ref 0 in (* last index *)
  let states = ref [] in
  let symbols = ref [] in
  let actions = ref [] in
  (try
    while true do
      match Parser.top Lexer.top lexbuf with
      | Grammar.SymbolDefinition (n, s) ->
        symbols := (n, expand_symbol !symbols s) :: !symbols
      | Grammar.ActionDefinition (n, a) ->
        actions := (n, expand_actions !actions a) :: !actions
      | Grammar.StateDefinition (s, t) ->
        if List.mem_assoc s !states then
          failwith @@ "state " ^ string_of_state s ^ " has been defined earlier"
        else
          let expand (s, i, a) =
            expand_symbol !symbols s,
            i,
            expand_actions !actions a in
          let t = List.map expand t in
          states := (s, t) :: !states;
          max_state := max (next_max_state !max_state t) (match s with
            | Grammar.StateName _ -> 0 (* neutral *)
            | Grammar.StateIndex n -> n);
      | Grammar.Empty ->
        ()
    done
  with End_of_file ->
    ());
  !states, !max_state

let compile lexbuf =
  (* conversion *)
  let states, assignments = parse lexbuf |> assign_ids in
  (* lots of calls *)
  let a = Fsa.create () in
  let add_state (i, t) =
    let add_transition (s, s', e) = Fsa.add a (i, s, s', e) in
    List.iter add_transition t in
  List.iter add_state states;
  (* done! *)
  a, assignments
