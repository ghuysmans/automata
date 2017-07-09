type 'a t =
  | Simple of 'a
  | Sequence of 'a t list
  | Union of 'a t list

let dump t =
  let rec f = function
    | Simple s -> s
    | Sequence l -> "(" ^ String.concat "; " (List.map f l) ^ ")"
    | Union l -> "(" ^ String.concat " || " (List.map f l) ^ ")" in
  Printf.printf "%s\n" (f t);
  t

let rec step test x t = match x with
  | Simple x ->
    Fsa.step test x t
  | Union l ->
    List.map (fun x -> step test x t) l |>
    List.concat
  | Sequence l ->
    List.fold_left (fun acc x ->
      if acc = [] then
        step test x t
      else
        acc
    ) [] l
