let client =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Unix.setsockopt fd Unix.SO_BROADCAST true;
  fd

let broadcast_address port =
  let ai = Unix.getaddrinfo "255.255.255.255" (string_of_int port) [] in
  (* FIXME? *)
  (List.hd ai).Unix.ai_addr

let dest_pool = ref []

let send port s =
  let dest =
    if List.mem_assoc port !dest_pool then
      List.assoc port !dest_pool
    else
      let d = broadcast_address port in
      dest_pool := (port, d) :: !dest_pool;
      d in
  Unix.sendto client s 0 (String.length s) [] dest |> ignore

let server port size f =
  let open Unix in
  let ai =
    let flags = [AI_PASSIVE; AI_SOCKTYPE SOCK_DGRAM] in
    getaddrinfo "0.0.0.0" (string_of_int port) flags in
  ai |> List.iter (fun a ->
    let fd = socket a.ai_family a.ai_socktype a.ai_protocol in
    bind fd a.ai_addr;
    let buffer = Bytes.create size in
    while true do
      let n = recv fd buffer 0 size [] in
      Bytes.sub_string buffer 0 n |> f
    done
  )
