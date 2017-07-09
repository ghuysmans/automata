{
  exception Unterminated_comment
  exception Unterminated_string

  let next_line lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      {
        pos with Lexing.pos_bol = lexbuf.Lexing.lex_curr_pos;
                 Lexing.pos_lnum = pos.Lexing.pos_lnum + 1
      }

  let inner_comments_number = ref 0
}


let lid = ['a'-'z']['a'-'z' '0'-'9' '_' '\'']*
let uid = ['A'-'Z']['A'-'Z' '0'-'9' '_' '\'']*
let num = '0' | '-'? ['1'-'9']['0'-'9']*
let newline = '\n'

rule top = parse
| [' ' '\t'] { top lexbuf }
| newline { next_line lexbuf; Parser.NL }
| "print" { Parser.PRINT }
| "'" [^'\''] "'" as s { Parser.CHAR s.[1] }
| "'\\n'" { Parser.CHAR '\n' }
| "'\\t'" { Parser.CHAR '\t' }
| "'\\\\'" { Parser.CHAR '\\' }
| "'\\''" { Parser.CHAR '\'' }
| "\"" { Parser.STRING (string lexbuf) }
| "broadcast" | "bc" { Parser.BROADCAST }
| lid as l { Parser.LOWER_ID l }
| "=" { Parser.EQUAL }
| "," { Parser.COMMA }
| uid as l { Parser.UPPER_ID l }
| num as n { Parser.INT (int_of_string n) }
| ":" { Parser.COLON }
| ";" { Parser.SEMICOLON }
| "-" { Parser.TO }
(* | "_" { Parser.ELSE } (* TODO add a default transition field *) *)
| "->" { Parser.ARROW }
| "(*" { comment lexbuf; top lexbuf }
| eof { Parser.EOF }

and comment = parse
| "*)" {
  if !inner_comments_number > 0 then (
    decr inner_comments_number;
    comment lexbuf
  ) }
| newline { next_line lexbuf; comment lexbuf }
| "(*" { incr inner_comments_number; comment lexbuf }
| eof { raise Unterminated_comment }
| _ { comment lexbuf }

(* FIXME inefficient but easy to read *)
and string = parse
| "\\n" { "\n" ^ string lexbuf }
| "\\t" { "\t" ^ string lexbuf }
| "\\\\" { "\\" ^ string lexbuf }
| "\\\"" { "\"" ^ string lexbuf }
| "\"" { "" }
| eof { raise Unterminated_string }
| _ as c { String.make 1 c ^ string lexbuf }
