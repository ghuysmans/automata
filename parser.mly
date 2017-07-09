%token PRINT
%token <string> STRING
%token BROADCAST
%token <string> LOWER_ID
%token EQUAL
%token <char> CHAR
%token COMMA
%token <string> UPPER_ID
%token <int> INT
%token COLON
%token SEMICOLON
%token TO
%token ARROW
%token NL
%token EOF

%start <Grammar.top> top
%%

symbol:
| i = LOWER_ID { Grammar.SymbolMacro i }
| c = CHAR { Grammar.Symbol c }
| c = CHAR ; TO ; c2 = CHAR { Grammar.SymbolRange (c, c2) }

symbols:
| s = symbol { [s] }
| s = symbol ; COMMA ; l = symbols { s :: l }

action:
| m = UPPER_ID { Grammar.ActionMacro m }
| PRINT ; s = STRING { Grammar.Action (Action.Print s) }
| BROADCAST ; p = INT ; s = STRING { Grammar.Action (Action.Broadcast (p, s)) }

actions:
| a = action { [a] }
| a = action ; COMMA ; l = actions { a :: l }

state:
| i = LOWER_ID { Grammar.StateName i }
| n = INT { Grammar.StateIndex n }

transition:
| s = symbols ; ARROW ; d = state ; SEMICOLON ; a = actions ; NL { Grammar.SymbolUnion s, d, a }

transitions:
| t = transition { [t] }
| t = transition ; l = transitions { t :: l }

top:
| NL { Grammar.Empty (* FIXME? *) }
| i = LOWER_ID ; EQUAL ; s = symbols ; NL { Grammar.SymbolDefinition (i, Grammar.SymbolUnion s) }
| i = UPPER_ID ; EQUAL ; a = actions ; NL { Grammar.ActionDefinition (i, a) }
| s = state ; COLON ; t = transitions ; NL { Grammar.StateDefinition (s, t) }
| EOF { raise End_of_file }
