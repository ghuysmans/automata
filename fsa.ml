type state = int
type ('sym, 'a) transition = 'sym * (state * 'a list)
type ('sym, 'a) t = {
  mutable (* there could be more *) states: ('sym, 'a) transition list array;
  mutable (* updated at each step *) state: state;
}

let create () = {
  states = [| [] |];
  state = 0;
}

let add x (s, sym, s', a) =
  let l = Array.length x.states in
  let m = 1 + max (max s s') l in
  if m > l then
    (* grow: copy the existing states and fill the rest with empty ones *)
    x.states <- Array.init m (fun i -> if i < l then x.states.(i) else []);
  (* update *)
  x.states <- Array.mapi (fun i st ->
    (* FIXME test intersections? *)
    if i <> s then
      (* we don't want to change any other state than the ith *)
      st
    else
      (* add the transition *)
      (sym, (s', a)) :: st
  ) x.states

let step test x sym =
  try
    let _, (s', a) = List.find (fun (s, _) -> test sym s) x.states.(x.state) in
    x.state <- s';
    a
  with Not_found ->
    x.state <- 0;
    []

module Int = struct
  type t = int
  let compare = compare
end
module S = Set.Make(Int)

let closure x i =
  let visited = ref S.empty in
  let l = Array.length x.states in
  let rec dfs i =
    if i<0 || i>=l then
      (* out of bounds *)
      Some i
    else if S.mem i !visited then
      (* already checked, we don't want to loop forever *)
      None
    else (
      visited := S.add i !visited;
      List.map (fun (_, (i', _)) -> dfs i') x.states.(i) |>
      List.fold_left (fun a b -> match a with
        | Some _ -> a
        | None -> b
      ) None
    ) in
  (* traverse the graph *)
  match dfs i with
    | Some bad -> `Invalid bad
    | None -> `Visited (S.elements !visited)

(* TODO remove useless states? use a map for the states we rename *)
(* TODO save as a list of tuples with the biggest index first to avoid O(n^2) *)
