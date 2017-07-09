type symbols =
  | SymbolUnion of symbols list
  | SymbolMacro of string
  | SymbolRange of char * char
  | Symbol of char

type action =
  | ActionMacro of string
  | Action of Action.t

type state_id =
  | StateName of string
  | StateIndex of int

type transition = symbols * state_id * action list

type top =
  | SymbolDefinition of string * symbols
  | ActionDefinition of string * action list
  | StateDefinition of state_id * transition list
