%token LPAR
%token RPAR
%token BEFORE
%token AND
%token <string> FILENAME
%token EOF

%start <string Combinators.t> top
%{
(* one level is enough (easy proof) *)
let simplify t = match t with
  | Combinators.Sequence [x] -> x
  | Combinators.Union [x] -> x
  | _ -> t
%}
%%

t:
| fn = FILENAME { Combinators.Simple fn }
| LPAR ; e = union ; RPAR { simplify (Combinators.Union e) }

seq:
| t = t { [t] }
| t = t ; BEFORE ; l = seq { t :: l }

union:
| s = seq { [simplify (Combinators.Sequence s)] }
| s = seq ; AND ; t = union { simplify (Combinators.Sequence s) :: t }

top:
| l = union ; EOF { simplify (Combinators.Union l) }
