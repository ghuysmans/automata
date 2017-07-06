type ('sym, 'a) t =
  | Simple of ('sym, 'a) Fsa.t
  | First of ('sym, 'a) t list
  | Both of ('sym, 'a) t list

let rec step x t = match x with
  | Simple x ->
    Fsa.step x t
  | Both l ->
    List.map (fun x -> step x t) l |>
    List.concat
  | First l ->
    List.fold_left (fun acc x ->
      if acc = [] then
        step x t
      else
        acc
    ) [] l
