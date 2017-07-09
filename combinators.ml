type ('sym, 'a) t =
  | Simple of ('sym, 'a) Fsa.t
  | First of ('sym, 'a) t list
  | Both of ('sym, 'a) t list

let rec step test x t = match x with
  | Simple x ->
    Fsa.step test x t
  | Both l ->
    List.map (fun x -> step test x t) l |>
    List.concat
  | First l ->
    List.fold_left (fun acc x ->
      if acc = [] then
        step test x t
      else
        acc
    ) [] l
