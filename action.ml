type t =
  | Print of string
  | Broadcast of int * string

let run = function
  | Print s ->
    print_endline s
  | Broadcast (port, s) ->
    Udp.send port s
