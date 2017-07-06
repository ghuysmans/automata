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
  x.states <- Array.mapi (fun i st ->
    if i <> s then
      (* we don't want to change any other state than the ith *)
      st
    else if List.mem_assoc sym st then
      failwith ("two transitions with the same symbol at " ^ string_of_int i)
    else
      (* add the transition *)
      (sym, (s', a)) :: st
  ) x.states

let step x sym =
  let s', a = List.assoc sym x.states.(x.state) in
  x.state <- s';
  a

module Int = struct
  type t = int
  let compare = compare
end
module S = Set.Make(Int)

let check x =
  let visited = ref S.empty in
  let l = Array.length x.states in
  let rec dfs i =
    if i<0 || i>=l then
      (* out of bounds *)
      false
    else if S.mem i !visited then
      (* already checked, we don't want to loop forever *)
      true
    else (
      visited := S.add i !visited;
      List.map (fun (_, (i', _)) -> dfs i') x.states.(i) |>
      List.fold_left (&&) true
    ) in
  dfs 0
