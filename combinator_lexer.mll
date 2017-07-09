rule top = parse
| [' ' '\t'] { top lexbuf }
| ";" | "->" | "<" { Combinator_parser.BEFORE }
| "(" { Combinator_parser.LPAR }
| ")" { Combinator_parser.RPAR }
| "||" { Combinator_parser.AND }
| ['0'-'9' 'A'-'Z' 'a'-'z' '.' '_']+ as fn { Combinator_parser.FILENAME fn }
| eof { Combinator_parser.EOF }
