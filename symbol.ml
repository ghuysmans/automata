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

(* TODO clean up this mess *)

let repr_char = function
  | '\n' -> "'\\n'"
  | '\t' -> "'\\t'"
  | '\\' -> "'\\\\'"
  | '\'' -> "'\\''"
  | c -> "'" ^ String.make 1 c ^ "'"

let repr2_char = function
  | '\n' -> "'\\\\n'"
  | '\t' -> "'\\\\t'"
  | '\\' -> "'\\\\'"
  | '\'' -> "'\''"
  | c -> "'" ^ String.make 1 c ^ "'"

let rec to_string = function
  | Grammar.SymbolUnion l -> String.concat ", " (List.map to_string l)
  | Grammar.SymbolMacro m -> m
  | Grammar.SymbolRange (a, b) -> repr2_char a ^ "-" ^ repr2_char b
  | Grammar.Symbol c' -> repr2_char c'
