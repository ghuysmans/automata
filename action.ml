type t =
  | Print of string
  | Broadcast of int * string

let run = function
  | Print s ->
    Printf.printf "%s\n" s
  | Broadcast (port, s) ->
    Udp.send port s;
    Printf.fprintf stderr "%s -> UDP %d\n" s port
