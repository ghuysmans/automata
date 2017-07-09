let rec test c = function
  | Grammar.SymbolUnion l ->
    List.map (fun s -> test c s) l |>
    List.exists (fun x -> x)
  | Grammar.SymbolMacro _ ->
    failwith "symbol macros should be resolved by now"
  | Grammar.SymbolRange (a, b) ->
    a <= c && c <= b
  | Grammar.Symbol c' ->
    c = c'
