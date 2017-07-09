type t =
  | Print of string
  | Broadcast of int * string

let dest_pool = ref []

let run = function
  | Print s ->
    Printf.printf "%s\n" s
  | Broadcast (p, s) ->
    let d =
      if List.mem_assoc p !dest_pool then
        List.assoc p !dest_pool
      else
        let d = Udp.create p in
        dest_pool := (p, d) :: !dest_pool;
        d in
    Udp.send d s;
    Printf.fprintf stderr "%s -> UDP %d\n" s p
